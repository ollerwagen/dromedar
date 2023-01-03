open Common
open Ast
open Astannotated
open Ll
open Builtins
open Typechecker

module Translator = struct

  (*
    Variable Context:
      Each local variable is stored as a pointer to a stack slot
      The Clang optimizer will remove unnecessary alloca-s

      Consider a variable x:int
      Ctxt contains binding  "x" -> %xsym, int, i64, where %sym :: i64*
  *)

  (*
    First string in the context corresponds to the current module
    Variable names in the context are mangled according to the module they are in: module$name
    Since only global variables matter for cross-module access, this doesn't cause any harm   
  *)
  module Ctxt = struct

    type t = string * (string * (Ast.ty * Ll.llty * Ll.operand)) list list

    let print_ctxt (c:t) : string =
      Printf.sprintf "Current Module: %s\n%s"
        (fst c) @@
        String.concat ";\n" @@
        List.map
          (fun l ->
            String.concat ", " @@
            List.map
              (fun (id,_) ->
                id
              )
              l
          )
          (snd c)

    let empty : t = default_module_name, [[]]

    let set_current_module (c:t) (m:string) : t = m, snd c

    let get_raw (c:t) (id:string) : Ast.ty * Ll.llty * Ll.operand =
      let found,res = List.fold_left (fun (f,r) c -> if f then true,r else if List.mem_assoc id c then true,List.assoc id c else f,r) (false,(TInt,Ll.I64,Ll.IConst 0L)) (snd c) in
      if found then res else raise Not_found

    let module_mangle (c:t) (id:string) : string = Printf.sprintf "%s$%s" (fst c) id

    let add_level (c:t) : t = fst c, [] :: (snd c)

    let top_level (c:t) = List.hd (snd c)

    let add_binding_to_module (c:t) (m:string) (id,bnd : string * (Ast.ty * Ll.llty * Ll.operand)) : t =
      fst c, ((Printf.sprintf "%s$%s" m id, bnd) :: List.hd (snd c)) :: List.tl (snd c)

    let add_binding (c:t) (b : string * (Ast.ty * Ll.llty * Ll.operand)) : t =
      add_binding_to_module c (fst c) b

    let get (c:t) (id:string) : Ast.ty * Ll.llty * Ll.operand =
      get_raw c @@ module_mangle c id
    
    let get_from_module (c:t) (m:string) (id:string) : Ast.ty * Ll.llty * Ll.operand =
      get_raw c @@ Printf.sprintf "%s$%s" m id

    let get_from_any_module (c:t) (id:string) : Ast.ty * Ll.llty * Ll.operand =
      let rec aux (c : (string * (Ast.ty * Ll.llty * Ll.operand)) list) =
        begin match c with
          | []           -> raise Not_found
          | (name,bnd)::cs ->
              let name' = List.nth (String.split_on_char '$' name) 1 in
              if id = name' then bnd else aux cs
        end
      in
      aux (top_level c)

  end

  (* local Dromedar instructions *)
  type instr =
    | I of Ll.instr
    | E of Ll.instr
    | L of string
    | T of Ll.term
    | G of Ll.ginstr
  
  type stream = instr list

  
  let gensym : string -> string =
    let n = ref 0 in
    (fun s -> incr n; Printf.sprintf "_%s%d" s (!n))


  let rec cmp_ty (t : Ast.ty) : Ll.llty =
    begin match t with
      | TInt                     -> I64
      | TFlt                     -> Double
      | TChar                    -> I8
      | TBool                    -> I1
      | TRef rt | TNullRef rt    -> Ptr (cmp_rty rt)
    end

  and cmp_rty (rt : Ast.rty) : Ll.llty =
    begin match rt with
      | TStr         -> cmp_rty (TArr TChar) (* strings are essentially just char-arrays *)
      | TArr t       -> Struct [ I64 ; Ptr (Array (0L, cmp_ty t)) ]
      | TFun (a,rt)  -> Func ((List.map cmp_ty a), cmp_retty rt)
      | TNamed _     -> I8
      | TModNamed _  -> I8
    end
  
  and cmp_retty (t : Ast.retty) : Ll.llty =
    begin match t with
      | Ast.Void  -> Ll.Void
      | Ast.Ret t -> cmp_ty t
    end

  (* context buildup with all builtin functions (see builtins.ml) *)
  let base_ctxt =
    List.fold_left (fun c (m,id,t,op) -> Ctxt.add_binding_to_module c m (id,(t, cmp_ty t, op))) Ctxt.empty builtins

  
  let allocate (size : Ll.operand) (t : Ll.llty) (name : string) : stream =
    let ptrsym = gensym "malloc" in
    [ I (Call (Some ptrsym, Ptr I8, Gid "_allocate", [ I64, size ]))
    ; I (Bitcast (name, Ptr I8, Id ptrsym, t))
    ]

  let addchild (from : llty * operand) (to_op : llty * operand) : stream =
    let bc1sym, bc2sym = gensym "addchild", gensym "addchild" in
    [ I (Bitcast (bc1sym, fst from, snd from, Ptr I8))
    ; I (Bitcast (bc2sym, fst to_op, snd to_op, Ptr I8))
    ; I (Call (None, Void, Gid "_addchild", [Ptr I8, Id bc1sym ; Ptr I8, Id bc2sym]))
    ]

  let removeref (r : llty * operand) : stream =
    let isym = gensym "removeref" in
    [ I (Bitcast (isym, fst r, snd r, Ptr I8))
    ; I (Call (None, Void, Gid "_removeref", [ Ptr I8, Id isym ]))
    ]

  let addref(r : llty * operand) : stream =
    let isym = gensym "addref" in
    [ I (Bitcast (isym, fst r, snd r, Ptr I8))
    ; I (Call (None, Void, Gid "_addref", [ Ptr I8, Id isym ]))
    ]

  let maybe_addref (t:ty) (r : llty * operand) : stream =
    begin match t with
      | TRef _ -> addref r
      | _      -> []
    end

  let maybe_removeref (t:ty) (r : llty * operand) : stream =
    begin match t with
      | TRef _ -> removeref r
      | _      -> []
    end

  let deptr (t:llty) : llty =
    begin match t with
      | Ptr t' -> t'
      | _      -> Stdlib.failwith "bad AST: should be pointer"
    end

  let cross_cast (et : ty) (lt,lllt,lop : ty * llty * operand) : (llty * operand) * stream =
    begin match et,lt with
      | TInt,TFlt | TFlt,TInt ->
          let rsym, c_et = gensym "crosscast", cmp_ty et in
          (c_et, Id rsym), [ I (Bitcast (rsym, lllt, lop, c_et)) ]
      | _ -> (lllt,lop), []
    end

  let double_to_i64 (op : operand) : operand * stream =
    let fptr, iptr, rsym = gensym "dti_ptr", gensym "dti_ptr_asi", gensym "dti_res" in
    Id rsym,
    [ E (Alloca (fptr, Double))
    ; I (Store (Double, op, Id fptr))
    ; I (Bitcast (iptr, Ptr Double, Id fptr, Ptr I64))
    ; I (Load (rsym, I64, Id iptr))
    ]


  (* last return value: true <=> value is linked to its own pref <=> needs to be GC'd *)
  let rec cmp_exp (c : Ctxt.t) (e : annt_exp) : operand * Ll.llty * stream * bool =

    let bop_ts : ((Ast.bop * Ast.ty * Ast.ty) * (Ast.ty * Ll.bop)) list =
      [ (Add,    TInt,  TInt ), (TInt,  Add )
      ; (Add,    TFlt,  TFlt ), (TFlt,  FAdd)
      ; (Add,    TChar, TChar), (TChar, Add )
      ; (Sub,    TInt,  TInt ), (TInt,  Sub )
      ; (Sub,    TFlt,  TFlt ), (TFlt,  FSub)
      ; (Sub,    TChar, TChar), (TChar, Sub )
      ; (Mul,    TInt,  TInt ), (TInt,  Mul )
      ; (Mul,    TFlt,  TFlt ), (TFlt,  FMul)
      ; (Div,    TInt,  TInt ), (TInt,  Div )
      ; (Div,    TFlt,  TFlt ), (TFlt,  FDiv)
      ; (Mod,    TInt,  TInt ), (TInt,  Rem )
      ; (Shl,    TInt,  TInt ), (TInt,  Shl )
      ; (Shr,    TInt,  TInt ), (TInt,  Shr )
      ; (Sha,    TInt,  TInt ), (TInt,  Sha )
      ; (Bitand, TInt,  TInt ), (TInt,  And )
      ; (Bitxor, TInt,  TInt ), (TInt,  Xor )
      ; (Bitor,  TInt,  TInt ), (TInt,  Or  )
      ; (Logxor, TBool, TBool), (TBool, Xor )
      ] in

    let str_bops : ((Ast.bop * Ast.ty * Ast.ty) * (string * Ast.ty)) list =
      [ (Add, TRef TStr, TRef TStr), ("_strconcat", TRef TStr)
      ; (Mul, TRef TStr, TInt),      ("_strmul_1",  TRef TStr)
      ; (Mul, TInt,      TRef TStr), ("_strmul_2",  TRef TStr) 
      ] in

    let bop_cast ((lt,lllt,lop) : ty * llty * operand) ((rt,rllt,rop) : ty * llty * operand) : (ty * llty * operand) * (ty * llty * operand) * stream =
      begin match lt,rt with
        | TInt,TInt | TFlt,TFlt | TBool,TBool | TChar,TChar -> (lt,lllt,lop), (rt,rllt,rop), []
        | TRef TStr, TRef TStr | TRef TStr, TInt | TInt, TRef TStr -> (lt,lllt,lop), (rt,rllt,rop), []
        | TInt,TChar ->
            let charcastop = gensym "charcast" in
            (rt, rllt, Id charcastop), (rt,rllt,rop), [ I (Bitcast (charcastop, lllt, lop, rllt)) ]
        | TChar,TInt ->
            let charcastop = gensym "charcast" in
            (lt,lllt,lop), (lt, lllt, Id charcastop), [ I (Bitcast (charcastop, rllt, rop, lllt)) ]
        | TInt,TFlt ->
            let fltcastop = gensym "fltcast" in
            (rt, rllt, Id fltcastop), (rt,rllt,rop), [ I (Bitcast (fltcastop, lllt, lop, rllt)) ]
        | TFlt,TInt ->
            let fltcastop = gensym "fltcast" in
            (lt,lllt,lop), (lt, lllt, Id fltcastop), [ I (Bitcast (fltcastop, rllt, rop, lllt)) ]
        | _ -> (lt,lllt,lop), (rt,rllt,rop), [] (* Stdlib.failwith "bop_cast: no cast found" *)
      end
    in

    let uop_ts : ((Ast.uop * Ast.ty) * (Ast.ty * Ll.bop * Ll.operand)) list =
      [ (Not, TBool), (TBool, Xor,  IConst 1L )
      ; (Neg, TInt ), (TInt,  Sub,  IConst 0L )
      ; (Neg, TFlt ), (TFlt,  FSub, FConst 0.0)
      ] in
    
    let pow_ts : ((Ast.ty * Ast.ty) * (Ast.ty * string)) list =
      [ (TInt, TInt), (TInt, "_pow_ii")
      ; (TFlt, TFlt), (TFlt, "_pow_ff")
      ] in
    
    let cmpop_to_ll : (Ast.cmpop * Ll.cmpop) list =
      [ Eq,        Eq
      ; Neq,       Neq
      ; Greater,   Greater
      ; GreaterEq, GreaterEq
      ; Less,      Less
      ; LessEq,    LessEq
      ; RefEq,     Eq
      ; RefNotEq,  Neq
      ] in

    begin match e with
      | Id id, t | ModAccess (_,id), t ->
          let op,llt,s,gc = cmp_lhs c e in
          begin match deptr llt with
            | Ptr (Func _) -> op, llt, s, false (* do not dereference functions *)
            | llt' ->
                let idsym = gensym id in
                Id idsym, llt', [ I (Load (idsym, llt', op)) ] @ gc, false
          end
      | LitInt  i, t  -> Ll.IConst i, cmp_ty t, [], false
      | LitFlt  f, t  -> Ll.FConst f, cmp_ty t, [], false
      | LitChar c, t  -> Ll.IConst (Int64.of_int @@ Char.code c), cmp_ty t, [], false
      | LitBool b, t  -> Ll.IConst (if b then 1L else 0L), cmp_ty t, [], false
      
      | LitStr  s, st ->
          let strlen = Int64.of_int (String.length s + 1) in
          let str_obj_ty, str_ty = cmp_ty st, Ptr (Array (0L, I8)) in
          let rsym, strsym = gensym "str", gensym "str" in
          let gsym, gsym_t = gensym "gstr", Array (strlen, I8) in
          let lsym = gensym "lstr" in
          let size_ptr, str_ptr = gensym "arrsize", gensym "arrdata" in
          let lsym_as_ptr, strsym_as_ptr = gensym "lsym_ptr", gensym "strsym_ptr" in
          let alloc_obj = allocate (IConst 16L) str_obj_ty rsym in
          let alloc_str = allocate (IConst strlen) str_ty strsym in

          Id rsym, str_obj_ty,

          (* allocate memory space for string structure and char array *)
          alloc_obj @ alloc_str @        
          (* add GC child from string structure to char array *)
          addchild (str_obj_ty, Id rsym) (str_ty, Id strsym) @
          removeref (str_ty, Id strsym) @
            (* create global string symbol *)
          [ G (GDecl (gsym, gsym_t, Str s))
            (* cast global char array to char pointer *)
          ; I (Bitcast (lsym, Ptr gsym_t, Gid gsym, str_ty))
            (* size_ptr points to size field in string structure *)
          ; I (Gep (size_ptr, str_obj_ty, Id rsym, [ IConst 0L; IConst 0L ]))
            (* str_ptr points to string field in string structure *)
          ; I (Gep (str_ptr, str_obj_ty, Id rsym, [ IConst 0L; IConst 1L ]))
            (* copy char array into string symbol *)
          ; I (Bitcast (lsym_as_ptr, str_ty, Id lsym, Ptr I8))
          ; I (Bitcast (strsym_as_ptr, str_ty, Id strsym, Ptr I8))
          ; I (Call (None, Void, Gid "_memcpy", [Ptr I8, Id lsym_as_ptr ; Ptr I8, Id strsym_as_ptr ; I64, IConst strlen]))
            (* store that pointer in the string pointer in the char structure *)
          ; I (Store (str_ty, Id strsym, Id str_ptr))
            (* store the size in the size field in the string structure *)
          ; I (Store (I64, IConst strlen, Id size_ptr))
          ],
          true
      
      | LitArr es, t ->
          let rsym, asym = gensym "array", gensym "array" in
          let size_ptr, arr_ptr = gensym "sizeptr", gensym "arrptr" in
          let arrlen = List.length es in
          let elemt, gcelems =
            begin match t with
              | TRef (TArr (TRef t)) -> TRef t, true
              | TRef (TArr t)        -> t,      false
              | _                    -> Stdlib.failwith "bad AST: array literal must be array"
            end in
          let c_t, c_et = cmp_ty t, cmp_ty elemt in
          let arr_elemt = Array (0L, c_et) in

          let alloc_obj = allocate (IConst 16L) c_t rsym in
          let alloc_arr = allocate (IConst (Int64.of_int (arrlen * size_ty c_et))) (Ptr arr_elemt) asym in

          Id rsym, c_t, 
          alloc_obj @ alloc_arr @
          addchild (c_t, Id rsym) (Ptr arr_elemt, Id asym) @
          removeref (Ptr arr_elemt, Id asym) @
          [ I (Gep (size_ptr, c_t, Id rsym, [ IConst 0L ; IConst 0L ]))
          ; I (Store (I64, IConst (Int64.of_int arrlen), Id size_ptr))
          ; I (Gep (arr_ptr, c_t, Id rsym, [ IConst 0L ; IConst 1L ]))
          ; I (Store (Ptr arr_elemt, Id asym, Id arr_ptr))
          ] @
          List.concat (
            List.mapi
              (fun i (e,t) ->
                (* compile the expression, store it at the appropriate index, add child and gc if necessary *)
                let op,llt,s,gc = cmp_exp c (e,t) in
                let addr_ptr = gensym "addr_ptr" in
                s @
                [ I (Gep (addr_ptr, Ptr arr_elemt, Id asym, [ IConst 0L ; IConst (Int64.of_int i) ]))
                ; I (Store (c_et, op, Id addr_ptr))
                ] @
                if gcelems then
                  addchild (c_t, Id rsym) (c_et,op) @ (if gc then removeref (c_et,op) else [])
                else
                  []
              )
              es
          ),
          true

      | EmptyList rt, t -> cmp_exp c (LitArr [], t)

      | RangeList (e1,i1,i2,e2), t ->
          let rsym = gensym "rangelist" in
          let (eop1,_,es1,_), (eop2,_,es2,_) = cmp_exp c e1, cmp_exp c e2 in
          Id rsym, cmp_ty t,
          es1 @ es2 @ [ I (Call (Some rsym, cmp_ty t, Gid "_makerangelist", [ I64, eop1 ; I64, eop2 ; I1, IConst (if i1=Incl then 1L else 0L) ; I1, IConst (if i2=Incl then 1L else 0L) ])) ],
          true

      | ListComp (e,vs,cnd), t ->

        (*
          let drmarr = gensym "arr" in

          let elem_t =
            begin match t with
              | TRef (TArr t) -> t
              | _             -> Stdlib.failwith "no array type in list comprehension"
            end in
          
          let arrdecl = VDecl (drmarr, Mut, None, (EmptyList elem_t, t)) in
          let loopcenter = [ If (cnd, [ Assn ((Id drmarr, t), (Bop (Add, (Id drmarr, t), (LitArr [e], t)), t)) ], []) ] in

          let loopstmts =
            List.fold_right
            (fun (id,(lexp,lt)) s ->
                let lname, iname = gensym (id^"list"), gensym "i" in
                let elem_t =
                  begin match lt with
                    | TRef (TArr t) -> t
                    | _ -> Stdlib.failwith "should be array in list comprehension expression"
                  end in
                [ VDecl (lname, Const, None, (lexp,lt))
                ; For (iname, (LitInt 0L, TInt), Incl, Excl, (Proj ((Id lname,lt), "length"), TInt),
                    [ VDecl (id, Const, None, (Subscript ((Id lname, lt), (Id iname, TInt)), elem_t)) ] @ s)
                ]
            )
            vs loopcenter
            in

          
          let allstmts = arrdecl :: loopstmts in
          let _ = Printf.printf "%s\n" (Astannotated.print_block 0 allstmts) in
          

          let c', declstream, _ = cmp_stmt Ast.Void c [] arrdecl in
          let _, loopstream = cmp_block Ast.Void c' [] loopstmts in

          let rsym = gensym "listcomp" in
          let (_,_,arrop) = Ctxt.get c' drmarr in
          
          Id rsym, cmp_ty t,
          declstream @ loopstream @ [ I (Load (rsym, cmp_ty t, arrop)) ],
          true
        *)
        
          let indexids = List.mapi (fun i _ -> gensym (Printf.sprintf "i%d" i)) vs in
          let valueids = List.map (fun (id,_) -> gensym id) vs in
          let looplbls = List.map (fun _ -> gensym "headx", gensym "bodyx", gensym "endx") vs in
          let lbladd, lblnoadd = gensym "add", gensym "noadd" in
          let rsym, rsym_cast, vecp = gensym "listcomp", gensym "listcompcast", gensym "listvec" in
          let handlegc_e =
            begin match snd e with
              | TRef _ | TNullRef _ -> true
              | _                   -> false
            end in

          let c' = List.fold_left
            (fun c ((id,(_,t)),vid) ->
              let t' =
                begin match t with
                  | TRef (TArr t) -> t
                  | _             -> Stdlib.failwith "bad AST: listcomp expression should be an array"
                end in
              Ctxt.add_binding c (id,(t', cmp_ty t', Id vid)))
            c (List.combine vs valueids) in

          let (eop,ellt,es,egc), (cop,cllt,cs,_) = cmp_exp c' e, cmp_exp c' cnd in
          let cd_ls = List.map (fun (_,e) -> cmp_exp c' e) vs in
          
          let e_as_int = gensym "exp_int" in

          Id rsym_cast, cmp_ty t,
          (List.map (fun id -> E (Alloca (id, I64))) indexids) @
          [ I (Call (Some vecp, Ptr I8, Gid "_make_vector", [])) ] @
          (List.concat (List.map
            (fun ((xid, iid), ((lop,lllt,ls,_), ((_, lexp), (lblhead, lblbody, lblend)))) ->
              let cmpval, listlen, listlenptr, elemlistptr, elemlistval, ival = gensym "listcomp_comparison", gensym "listlen", gensym "listlenptr", gensym "elemlistptr", gensym "elemlistval", gensym "indexval" in
              let elem_t =
                begin match snd lexp with
                  | TRef (TArr t) -> t
                  | _             -> Stdlib.failwith "bad AST: non-array type for comprehension list element list"
                end in
              ls @
              [ I (Gep (listlenptr, lllt, lop, [ IConst 0L ; IConst 0L ]))
              ; I (Load (listlen, I64, Id listlenptr))
              ; I (Store (I64, IConst 0L, Id iid))
              ; T (Br lblhead)
              ; L lblhead
              ; I (Load (ival, I64, Id iid))
              ; I (Cmp (cmpval, ICmp, Less, I64, Id ival, Id listlen))
              ; T (Cbr (Id cmpval, lblbody, lblend))
              ; L lblbody
              ; I (Gep (elemlistptr, lllt, lop, [ IConst 0L ; IConst 1L ]))
              ; I (Load (elemlistval, Ptr (Array (0L, cmp_ty elem_t)), Id elemlistptr))
              ; I (Gep (xid, Ptr (Array (0L, cmp_ty elem_t)), Id elemlistval, [ IConst 0L ; Id ival ]))
              ]
            )
            (List.combine (List.combine valueids indexids) (List.combine cd_ls (List.combine vs looplbls)))
          )) @
          cs @
          [ T (Cbr (cop, lbladd, lblnoadd))
          ; L lbladd
          ] @
          es @
          (if snd e = TFlt then
            let op,caststream = double_to_i64 eop in
            caststream @ [ I (Call (None, Void, Gid "_addelem", [ Ptr I8, Id vecp ; I64, op ])) ]
          else
            [ I (Bitcast (e_as_int, ellt, eop, I64))
            ; I (Call (None, Void, Gid "_addelem", [ Ptr I8, Id vecp ; I64, Id e_as_int ])) ]
          ) @
          (if handlegc_e then
            addchild (Ptr I8, Id vecp) (ellt, eop) @ if egc then removeref (ellt, eop) else []
          else []) @
          [ T (Br lblnoadd) ; L lblnoadd ] @
          (List.concat (List.rev (List.map
            (fun ((iid,(lblhead,_,lblend)), (lop,lllt,_,lgc)) ->
              let ival, iincval = gensym "ix", gensym "ixplus" in
              [ I (Load (ival, I64, Id iid))
              ; I (Binop (iincval, Add, I64, IConst 1L, Id ival))
              ; I (Store (I64, Id iincval, Id iid))
              ; T (Br lblhead)
              ; L lblend
              ] @
              (if lgc then removeref (lllt,lop) else [])
            )
            (List.combine (List.combine indexids looplbls) cd_ls)
          ))) @
          [ I (Call (Some rsym, Ptr (Struct [ I64 ; Ptr (Array (0L, I8))]), Gid "_genlist", [ Ptr I8, Id vecp ; I64, IConst (Int64.of_int (size_ty (cmp_ty (snd e)))) ; I1, IConst (if handlegc_e then 1L else 0L) ]))
          ; I (Bitcast (rsym_cast, Ptr (Struct [ I64 ; Ptr (Array (0L, I8))]), Id rsym, cmp_ty t))
          ],
          true
        

      | Ternary (cnd,e1,e2), t ->
          let rsym, rptr = gensym "ternres", gensym "ternptr" in
          let lbltrue, lblfalse, lblend = gensym "terntrue", gensym "ternfalse", gensym "ternend" in
          let (cndop,cndllt,cnds,cndgc), (eop1,ellt1,es1,egc1), (eop2,ellt2,es2,egc2) = cmp_exp c cnd, cmp_exp c e1, cmp_exp c e2 in
          let addref1, addref2 = not egc1 && egc2, egc1 && not egc2 in
          Id rsym, cmp_ty t,
          [ E (Alloca (rptr, cmp_ty t)) ] @ cnds @
          [ T (Cbr (cndop, lbltrue, lblfalse))
          ; L lbltrue ] @ es1 @ (if addref1 then addref (ellt1,eop1) else []) @ [ I (Store (ellt1, eop1, Id rptr)) ; T (Br lblend) ] @
          [ L lblfalse ] @ es2 @ (if addref2 then addref (ellt2,eop2) else []) @ [ I (Store (ellt2, eop2, Id rptr)) ; T (Br lblend)
          ; L lblend ; I (Load (rsym, cmp_ty t, Id rptr)) ],
          egc1 || egc2

      | Null rt, t      -> Null, cmp_ty t, [], false

      | Sprintf (opt,s,es), t ->

          let rec walk_arr (t : ty) : int * ty =
            begin match t with
              | TRef (TArr t) -> let d,t = walk_arr t in d+1, t
              | _             -> 0, t
            end
          in

          let cmpd_es = List.map (cmp_exp c) es in
          let substrings = Str.full_split (Str.regexp "{[0-9]}") s in
          let arggcs = List.concat (List.map (fun (op,llt,_,gc) -> if gc then removeref (llt,op) else []) cmpd_es) in
          let strty = cmp_ty (TRef TStr) in
          let args_cmpd = List.concat @@ List.map (fun (_,_,s,_) -> s) cmpd_es in
          let makestr_instrs =
            List.map
            (function
              | Str.Text s ->
                  let op,_,instrs,gc = cmp_exp c (LitStr s, TRef TStr) in
                  op, instrs, gc
              | Str.Delim s ->
                  let index = Stdlib.int_of_string @@ String.sub s 1 @@ String.length s - 2 in
                  let (op,llt,_,gc), (_,t) = List.nth cmpd_es index, List.nth es index in
                  let rsym, casted_op = gensym "op", gensym "casted_sprintf_op" in
                  begin match t with
                    | TRef TStr -> op, [], false
                    | TRef (TArr t) ->
                        let arrdepth, lowest_ty = walk_arr t in
                        let lowest_function =
                          begin match lowest_ty with
                            | TInt  -> "_sprintf_int"
                            | TFlt  -> "_sprintf_flt"
                            | TChar -> "_sprintf_char"
                            | TBool -> "_sprintf_bool"
                            | TRef TStr -> "_sprintf_str"
                            | _     -> Stdlib.failwith "cannot print this type"
                          end in
                        Id rsym, [ I (Bitcast (casted_op, llt, op, I64)) ; I (Call (Some rsym, strty, Gid "_sprintf_array", [ I64, Id casted_op ; I64, IConst (Int64.of_int (arrdepth + 1)) ; I64, IConst (Int64.of_int (size_ty (cmp_ty lowest_ty))) ; Ptr (Func ([I64], strty)), Gid lowest_function ])) ], true
                    | TInt      -> Id rsym, [ I (Call (Some rsym, strty, Gid "_sprintf_int",  [ I64, op ])) ], true
                    | TFlt      ->
                        let castop,casts = double_to_i64 op in
                        Id rsym, casts @ [ I (Call (Some rsym, strty, Gid "_sprintf_flt",  [ I64, castop ])) ], true
                    | TChar     -> Id rsym, [ I (Bitcast (casted_op, llt, op, I64)) ; I (Call (Some rsym, strty, Gid "_sprintf_char", [ I64, Id casted_op ])) ], true
                    | TBool     -> Id rsym, [ I (Bitcast (casted_op, llt, op, I64)) ; I (Call (Some rsym, strty, Gid "_sprintf_bool", [ I64, Id casted_op ])) ], true
                    | _         -> Stdlib.failwith "bad sprintf type"
                  end
            )
            substrings in
          let makestr_streams = List.concat ((List.map (fun (_,s,_) -> s)) makestr_instrs) in
          let strgcs = List.concat (List.filter_map (fun (op,_,gc) -> if gc then Some (removeref (strty,op)) else None) makestr_instrs) in
          let catargs = List.map (fun (op,_,_) -> strty, op) makestr_instrs in
          let rsym = gensym "sprintf_res" in
          let printf_call =
            begin match opt with
              | Sprintf -> []
              | Printf  -> [ I (Call (None, Void, Gid "_IO$print_str", [ strty, Id rsym ])) ] @ removeref (strty, Id rsym)
            end in
          Id rsym, strty, (args_cmpd @ makestr_streams @ [ I (Call (Some rsym, Func ([ I64; VariadicDots ], strty), Gid "_sprintf_cat", (I64, IConst (Int64.of_int (List.length catargs))) :: catargs)) ] @ printf_call @ arggcs @ strgcs), true

      | Bop (op,l,r), t -> (* no GC, as all input results are primitives *)
          (* ignore gc as all inputs are primitives *)
          let rsym = gensym "binop" in
          let (op1,llt1,s1,gc1), (op2,llt2,s2,gc2) = cmp_exp c l, cmp_exp c r in

          let gcops = (if gc1 then removeref (llt1,op1) else []) @ (if gc2 then removeref (llt2,op2) else []) in

          begin match snd l, snd r with
            | TRef TStr, TRef TStr | TRef TStr, TInt | TInt, TRef TStr ->
                let fname,rt = List.assoc (op,snd l,snd r) str_bops in
                Id rsym, cmp_ty rt,
                s1 @ s2 @ [ I (Call (Some rsym, cmp_ty rt, Gid fname, [ llt1, op1 ; llt2, op2 ]))] @ gcops, true
            | TRef (TArr t1), TRef (TArr t2) ->
                let elemsize =
                  begin match llt1 with
                    | Ptr (Struct [_; Ptr (Array (_, llt))]) -> size_ty llt
                    | _ -> Stdlib.failwith "bad AST: not array return type"
                  end in
                let reduced1, reduced2, concatres = gensym "arrcatredop", gensym "arrcatredop", gensym "concat_arr" in
                let reduced_t = cmp_ty (TRef (TArr TChar)) in
                Id rsym, cmp_ty t,
                s1 @ s2 @
                  [ I (Bitcast (reduced1, llt1, op1, reduced_t)) ; I (Bitcast (reduced2, llt2, op2, reduced_t))
                  ; I (Call (Some concatres, reduced_t, Gid "_arrconcat", [ reduced_t, Id reduced1 ; reduced_t, Id reduced2 ; I64, IConst (Int64.of_int elemsize) ; I1, IConst (begin match t1 with | TRef _ -> 1L | _ -> 0L end) ]))
                  ; I (Bitcast (rsym, reduced_t, Id concatres, cmp_ty t))
                  ] @ gcops,
                true
            | _ ->
                let (lt,llt1,op1), (rt,llt2,op2), caststream = bop_cast (snd l,llt1,op1) (snd r,llt2,op2) in
                begin match op with
                  | Pow ->
                      let rt,fname = List.assoc (lt,rt) pow_ts in
                      Id rsym, cmp_ty rt, s1 @ s2 @ caststream @ [ I (Call (Some rsym, cmp_ty rt, Gid fname, [ llt1, op1; llt2, op2 ])) ], false
                  | Logand | Logor ->
                      let shortcircuiteval, shortcircuitstore =
                        if op = Logand then 0L, 0L else 1L, 1L in
                      let cmpres = gensym "and" in
                      let shortcircuit, evalboth, logend = gensym "shortcircuit", gensym "evalboth", gensym "logend" in
                      let resstack = gensym "logstack" in
                      Id rsym, cmp_ty TBool,
                      [ E (Alloca (resstack, I1)) ] @
                      s1 @
                      [ I (Cmp (cmpres, ICmp, Eq, I1, op1, IConst shortcircuiteval))
                      ; T (Cbr (Id cmpres, shortcircuit, evalboth))
                      ; L shortcircuit
                      ; I (Store (I1, IConst shortcircuitstore, Id resstack))
                      ; T (Br logend)
                      ; L evalboth
                      ] @
                      s2 @
                      [ I (Store (I1, op2, Id resstack))
                      ; T (Br logend)
                      ; L logend
                      ; I (Load (rsym, I1, Id resstack))
                      ],
                      false
                  | _ ->
                      let rt,llop = List.assoc (op,lt,rt) bop_ts in
                      Id rsym, cmp_ty rt, s1 @ s2 @ caststream @ [ I (Binop (rsym, llop, cmp_ty rt, op1, op2))], false
                end
          end

      | Uop (op,e), t -> (* no GC, as all input results are primitives *)
          (* ignore gc as all inputs are primitives *)
          let eop,llt,s,gc = cmp_exp c e in
          let rsym = gensym "uop" in
          let rt,llop,argop = List.assoc (op, snd e) uop_ts in
          Id rsym, cmp_ty rt, s @ [ I (Binop (rsym, llop, cmp_ty rt, argop, eop)) ], false

      | Cmps (f,rs), t -> (* no GC, as all input results are primitives *)
          let rec genislastlist (l : 'a list) : ('a * bool) list =
            begin match l with
              | []  -> Stdlib.failwith "bad AST: empty comparison value list"
              | [x] -> [x,true]
              | x::xs -> (x,false) :: genislastlist xs
            end
          in

          let isnonrefop (op : Ast.cmpop) = op <> RefEq && op <> RefNotEq in
          let res, resstack = gensym "cmp", gensym "cmp" in
          (* ignore gc as all inputs are primitives *)
          let fop,fllt,fs,gc = cmp_exp c f in
          let fstlbl, lblfalse, lblend = gensym "cmplbl", gensym "cmpfalse", gensym "cmpend" in
          let lastop,last_t,lastlbl,lastgc,s =
            List.fold_left
              (fun (lop,lt,lbl,lgc,s) ((op,rexp),islast) ->
                (* ignore gc as all inputs are primitives *)
                let rop,rllt,rs,rgc = cmp_exp c rexp in
                let (lt,lllt,lop), (rt,rllt,rop), caststream = bop_cast (lt,cmp_ty lt,lop) (snd rexp,rllt,rop) in
                let tmpflslbl, nextlbl, nextres = gensym "tmpfls", gensym "cmplbl", gensym "cmp" in
                let cmpop = List.assoc op cmpop_to_ll in
                let cmpstream =
                  begin match lt, snd rexp, isnonrefop op with
                    | TRef TStr, TRef TStr, true ->
                        let cmpres = gensym "cmpstr" in
                        [ I (Call (Some cmpres, I64, Gid "_strcmp", [ lllt, lop ; rllt, rop ]))
                        ; I (Cmp (nextres, ICmp, cmpop, I64, Id cmpres, IConst 0L)) ]
                    | _ ->
                        let cmpty =
                          begin match lt with
                            | TFlt -> FCmp
                            | _    -> ICmp (* ints and chars, faulty types removed by typechecker *)
                          end in
                        [ I (Cmp (nextres, cmpty, cmpop, rllt, lop, rop)) ]
                  end in
                rop, snd rexp, nextlbl, gc,
                s @ [ L lbl ] @ rs @ caststream @ cmpstream @
                (if lgc then removeref (lllt,lop) else []) @ (* have to gc the rhs if comparison is false *)
                (if rgc && islast then removeref (rllt,rop) else []) @
                [ T (Cbr (Id nextres, nextlbl, tmpflslbl)) ; L tmpflslbl] @
                (if not islast && rgc then removeref (rllt, rop) else []) @
                [ T (Br lblfalse) ]
              )
              (fop,snd f,fstlbl,gc,[]) (genislastlist rs) in
          Id res, cmp_ty TBool,
          [ E (Alloca (resstack, cmp_ty TBool)) ] @ fs @ [ T (Br fstlbl) ] @ s @
          [ L lastlbl ; I (Store (cmp_ty TBool, IConst 1L, Id resstack)) ; T (Br lblend) ;
            L lblfalse ; I (Store (cmp_ty TBool, IConst 0L, Id resstack)) ; T (Br lblend) ;
            L lblend ; I (Load (res, cmp_ty TBool, Id resstack)) ],
          false

      | FApp (f,a), t ->
          let getstream (_,_,s,_) = s in
          (* fgc must be = false since functions are not garbage-collectable (yet) *)
          let (fop,fllt,fs,fgc), argcs = cmp_exp c f, List.map (cmp_exp c) a in
          let arggcops = List.concat (List.map2 (fun (op,llt,_,gc) (_,t) -> if gc then removeref (llt,op) else []) argcs a) in
          let rsym = gensym "callop" in
          begin match snd f with
            | TRef (TFun (argts,rt)) ->
                let llrt = cmp_retty rt in
                let args_casts =
                  List.map2
                    (fun ((op,llt,_,_),(_,pt)) et -> cross_cast et (pt,llt,op))
                    (List.combine argcs a) argts in
                let argcs', caststreams = List.map fst args_casts, List.concat @@ List.map snd args_casts in
                Id rsym, llrt,
                fs @ List.concat (List.map getstream argcs) @ caststreams @
                [ I (Call (Some rsym, llrt, fop, argcs')) ] @ arggcops,
                begin match t with | TRef _ | TNullRef _ -> true | _ -> false end
            | _ -> Stdlib.failwith "not a function, abort"
          end

      | Subscript (l,i), t ->
          let ptrop, pllt, s, gc = cmp_lhs c (Subscript (l,i), t) in
          let rsym = gensym "subscript" in
          let must_gc = begin match t with | TRef _ -> true | _ -> false end in
          Id rsym, cmp_ty t, s @ [ I (Load (rsym, cmp_ty t, ptrop)) ] @ (if must_gc then addref (cmp_ty t, Id rsym) else []) @ gc, must_gc
      
      | Proj (lhs,id), t ->
          let op,llt,s,gc = cmp_exp c lhs in
          begin match (snd lhs), id with
            | TRef (TArr t), "length" ->
                let rptr, rsym = gensym "lenptr", gensym "lenval" in
                Id rsym, I64,
                s @ [ I (Gep (rptr, llt, op, [ IConst 0L ; IConst 0L ])) ; I (Load (rsym, I64, Id rptr)) ] @ (if gc then removeref (llt,op) else []),
                false
            | _ -> Stdlib.failwith "bad AST: bad projection type/id"
          end
    end
  
  (* second stream result is the garbage collection stream *)
  and cmp_lhs (c : Ctxt.t) (e : annt_exp) : operand * Ll.llty * stream * stream =
    begin match e with
      | Id id, t ->
          let t,llt,op = Ctxt.get c id in
          op, Ptr llt, [], []

      | ModAccess (m,id), t ->
          let t,llt,op = Ctxt.get_from_module c m id in
          op, Ptr llt, [], []

      | Subscript (b,o), t ->
          let (bop,bllt,bs,bgc), (oop,ollt,os,_) = cmp_exp c b, cmp_exp c o in
          let c_t, c_et = cmp_ty (snd b), cmp_ty t in
          let rsym, arr_ptr, arr_op = gensym "subscript", gensym "sub_arr_ptr", gensym "sub_arr_op" in
          Id rsym, Ptr (cmp_ty t),
          bs @ os @
          [ I (Gep (arr_ptr, c_t, bop, [ IConst 0L ; IConst 1L ]))
          ; I (Load (arr_op, Ptr (Array (0L, c_et)), Id arr_ptr))
          ; I (Gep (rsym, Ptr (Array (0L, c_et)), Id arr_op, [ IConst 0L ; oop ]))
          ],
          (if bgc then removeref (bllt,bop) else [])

      | _     -> Stdlib.failwith "lhs unimplemented"
    end
  
  (* string list is list of all variables that need to be gc'd at a return statement *)
  and cmp_stmt (rt : Ast.retty) (c : Ctxt.t) (refvars : (llty * operand) list) (s : annt_stmt) : Ctxt.t * stream * (llty * operand) list =
    begin match s with
      | VDecl (id, _, t, e) ->
          let op,ellt,s,gc = cmp_exp c e in
          let vt, ((ellt,op),crosscaststream) =
            begin match t with
              | None   -> snd e, ((ellt,op),[])
              | Some t -> t,     cross_cast t (snd e, ellt, op)
            end in
          let vllt = cmp_ty vt in
          let llid = gensym id in

          (* alloca and store expression result -> does this ever need a bitcast from ellt to vllt? *)
          (* store variables as pointers in context *)
          Ctxt.add_binding c (id, (vt, vllt, Id llid)),
          s @ crosscaststream @ [ E (Alloca (llid, vllt)) ; I (Store (ellt, op, Id llid)) ] @
            begin match vt,gc with
              | TRef _, false | TNullRef _, false -> addref (ellt,op)
              | _             -> []
            end,
          begin match vt with
            | TRef _ | TNullRef _ -> (vllt, Id llid) :: refvars
            | _      -> refvars
          end
          (* add pref and remove it from %op: cancel each other out *)
          
      | Assert e ->
          let lbltrue, lblfalse = gensym "assert_true", gensym "assert_false" in
          let op,llt,s,_ = cmp_exp c e in
          let failstring = Printf.sprintf "Assertion failure in {%s}\nAborting.\n" (Astannotated.print_exp e) in
          let fop,fllt,fs,fgc = cmp_exp c (LitStr failstring, TRef TStr) in
          c,
          s @
          [ T (Cbr (op, lbltrue, lblfalse)) ; L lblfalse ] @
          fs @
          [ I (Call (None, Void, Gid "print_str", [ fllt, fop ]))
          ; I (Call (None, Void, Gid "_abort", [ I64, IConst 134L ]))
          ; T (Br lbltrue)
          ] @
          [ L lbltrue ],
          refvars

      | Assn (l,r) ->
          let (lop,lllt,ls,lgc), (rop,rllt,rs,rgc) = cmp_lhs c l, cmp_exp c r in
          let (rllt,rop),crosscaststream = cross_cast (snd l) (snd r, rllt, rop) in
          let gc_prevval =
            begin match snd l with
              | TRef _ ->
                  let gcobj, lllt' = gensym "gc_prevval", deptr lllt in
                  [ I (Load (gcobj, lllt', lop)) ] @ removeref (lllt',Id gcobj) @ (if rgc then [] else addref (rllt, rop))
              | _ -> [] (* do not attempt to gc primitives *)
            end in
          c, ls @ rs @ crosscaststream @ gc_prevval @ [ I (Store (rllt, rop, lop)) ] @ lgc, refvars
      | Expr (e,t) ->
          begin match e with
            | Sprintf (Printf,s,es) -> let _,_,s,_ = cmp_exp c (e, TRef TStr) in c, s, refvars
            | FApp (f,a) ->
                let getstream (_,_,s,_) = s in
                (* fgc must be = false since functions are not garbage-collectable (yet) *)
                let (fop,fllt,fs,fgc), argcs = cmp_exp c f, List.map (cmp_exp c) a in
                let arggcops = List.concat (List.map2 (fun (op,llt,_,gc) (_,t) -> if gc then removeref (llt,op) else []) argcs a) in
                begin match snd f with
                  | TRef (TFun (argts,rt)) ->
                      let removerefretstream, retval =
                        begin match rt with 
                          | Ret (TRef t) ->
                              let ret_name, cd_retty = gensym "ret_ref", cmp_retty (Ret (TRef t)) in
                              removeref (cd_retty, Id ret_name), Some ret_name
                          | _ -> [], None
                        end in
                      let llrt = cmp_retty rt in
                      let args_casts =
                        List.map2
                          (fun ((op,llt,_,_),(_,pt)) et -> cross_cast et (pt,llt,op))
                          (List.combine argcs a) argts in
                      let argcs', caststreams = List.map fst args_casts, List.concat @@ List.map snd args_casts in
                      c,
                      fs @ List.concat (List.map getstream argcs) @ caststreams @
                      [ I (Call (retval, llrt, fop, argcs')) ] @ removerefretstream @ arggcops,
                      refvars
                  | _ -> Stdlib.failwith "not a function, abort"
                end
            | _ -> Stdlib.failwith "can only compile function call as expression statement"
          end
      | If (cnd,t,nt) -> (* conditionals are bool -> primitive -> not GC'able (same for while, do-while) *)
          let cop,_,cs,_ = cmp_exp c cnd in
          let (_,s1), (_,s2) = cmp_block rt c refvars t, cmp_block rt c refvars nt in
          let lbl1, lbl2, lblend = gensym "if_lbl", gensym "if_lbl", gensym "ifi_lbl_end" in
          c, cs @ [ T (Cbr (cop, lbl1, lbl2)) ; L lbl1 ] @ s1 @ [ T (Br lblend) ; L lbl2 ] @ s2 @ [ T (Br lblend) ; L lblend ], refvars
      | Denull (id,e,t,nt) ->
          let nnsym, cmpres = gensym id, gensym "cmpres" in
          let ifnonnull, ifnull, lblend = gensym "denull_lbl_nonnull", gensym "denull_lbl_null", gensym "denull_end" in
          let eop,ellt,es,egc = cmp_exp c e in
          let c' = Ctxt.add_level @@ Ctxt.add_binding (Ctxt.add_level c) (id, (snd e, ellt, Id nnsym)) in
          let (_,s1), (_,s2) = cmp_block rt c' refvars t, cmp_block rt c refvars nt in
          c,
          es @ [ E (Alloca (nnsym, ellt)) ; I (Cmp (cmpres, ICmp, Eq, ellt, eop, Null)) ; T (Cbr (Id cmpres, ifnull, ifnonnull)) ; L ifnonnull ; I (Store (ellt, eop, Id nnsym)) ] @
            s1 @ [ T (Br lblend) ; L ifnull ] @ s2 @ [ T (Br lblend) ; L lblend ], refvars
      | While (cnd,b) ->
          let cop,_,cs,_ = cmp_exp c cnd in
          let _,s = cmp_block rt c refvars b in
          let lblstart, lblbody, lblend = gensym "while_lbl", gensym "while_lbl", gensym "while_lbl" in
          c, [ T (Br lblstart) ; L lblstart ] @ cs @ [ T (Cbr (cop, lblbody, lblend)) ; L lblbody ] @ s @ [ T (Br lblstart); L lblend ], refvars
      | DoWhile (cnd,b) ->
          let cop,_,cs,_ = cmp_exp c cnd in
          let _,s = cmp_block rt c refvars b in
          let lblstart, lblend = gensym "dowhile_lbl", gensym "dowhile_lbl" in
          c, [ T (Br lblstart) ; L lblstart ] @ s @ cs @ [ T (Cbr (cop, lblstart, lblend)) ; L lblend ], refvars
      | For (id,s,incl1,incl2,e,b) ->
          let diff, i = gensym "diff", gensym "for_var" in
          let lbl_cmp_up, lbl_cmp_dn = gensym "lbl_cmp_up", gensym "lbl_cmp_dn" in
          let lblbody, lblend = gensym "for_start", gensym "for_end" in
          let cmp_up_i, cmp_up, cmp_dn_i, cmp_dn = gensym "cmp_up_i", gensym "cmp_up", gensym "cmp_dn_i", gensym "cmp_dn" in
          let incr_i, incrd_i = gensym "for_inc_i", gensym "for_incd_i" in
          let calc_step_up, calc_step_dn, after_calc_step = gensym "for_calc_up", gensym "for_calc_dn", gensym "for_calc_after" in 
          let step_ptr, step = gensym "for_step_ptr", gensym "step" in
          let lblstart = gensym "for_start" in

          let c' = Ctxt.add_binding c (id, (TInt, I64, Id i)) in

          (* bounds are of type int -> not GC'able (primitive) *)
          let (sop,_,ss,_), (eop,_,es,_) = cmp_exp c s, cmp_exp c e in
          let _,bs = cmp_block rt c' refvars b in

          c,
          ss @ es @
          [ I (Cmp (diff, ICmp, LessEq, I64, sop, eop))
          ; T (Cbr (Id diff, calc_step_up, calc_step_dn))
          ; E (Alloca (step_ptr, I64))
          ; L calc_step_up
          ; I (Store (I64, IConst 1L, Id step_ptr))
          ; T (Br after_calc_step)
          ; L calc_step_dn
          ; I (Store (I64, IConst (-1L), Id step_ptr))
          ; T (Br after_calc_step)
          ; L after_calc_step
          ; I (Load (step, I64, Id step_ptr))

          ; E (Alloca (i, I64))
          ] @
          (if incl1=Incl then
            [ I (Store (I64, sop, Id i)) ]
          else
            let incd_s = gensym "for_var_inc" in
            [ I (Binop (incd_s, Add, I64, Id step, sop))
            ; I (Store (I64, Id incd_s, Id i))
            ]
          ) @
          [ T (Br lblstart)
          ; L lblstart
          ; T (Cbr (Id diff, lbl_cmp_up, lbl_cmp_dn))
          ; L lbl_cmp_up
          ; I (Load (cmp_up_i, I64, Id i))
          ; I (Cmp (cmp_up, ICmp, (if incl2=Incl then LessEq else Less), I64, Id cmp_up_i, eop))
          ; T (Cbr (Id cmp_up, lblbody, lblend))
          ; L lbl_cmp_dn 
          ; I (Load (cmp_dn_i, I64, Id i))
          ; I (Cmp (cmp_dn, ICmp, (if incl2=Incl then GreaterEq else Greater), I64, Id cmp_dn_i, eop))
          ; T (Cbr (Id cmp_dn, lblbody, lblend))
          ; L lblbody
          ] @ bs @
          [ I (Load (incr_i, I64, Id i))
          ; I (Binop (incrd_i, Add, I64, Id incr_i, Id step))
          ; I (Store (I64, Id incrd_i, Id i))
          ; T (Br lblstart)
          ; L lblend
          ],
          refvars

      | Return e ->
          let free_vars = List.concat (List.map
            (fun (llt,op) ->
                let refptr = gensym "returndel" in
                I (Load (refptr, llt, op)) :: removeref (llt, Id refptr)
            )
            refvars
          ) in
          begin match e with
            | None   -> c, free_vars @ [ T (Ll.Ret None) ], refvars
            | Some e ->
                begin match rt with
                  | Ret rt ->
                      let op,lt,s,gc = cmp_exp c e in
                      let (lt',op'),caststream = cross_cast rt (snd e, lt, op) in
                      c, s @ (if gc then [] else maybe_addref (snd e) (lt,op)) @ free_vars @ caststream @ [ T (Ret (Some (lt', op'))) ], refvars
                  | Void -> Stdlib.failwith "bad AST: return-value statement in void function"
                end
          end
    end
  and cmp_block (rt : Ast.retty) (c : Ctxt.t) (refvars : (llty * operand) list) (b : annt_stmt list) : Ctxt.t * stream =
    let c',s,rv' = List.fold_left (fun (c,s,rv) st -> let c',sta,rv' = cmp_stmt rt c rv st in c',s@sta,rv') (Ctxt.add_level c,[],refvars) b in
    c', s @ List.concat (List.map (fun (_,(_,llt,op)) -> let refptr = gensym "returndel" in I (Load (refptr, llt, op)) :: removeref (llt, Id refptr)) (Ctxt.top_level c'))
  
  let make_cfg (s : stream) : ginstr list * (Ll.firstblock * Ll.block list) =
    let g,ei,et,cl,ci,bs =
      List.fold_left
        (fun (g,ei,et,cl,ci,bs) strelem ->
          begin match strelem with
            | I is -> g,ei,et,cl,ci@[is],bs
            | G gi -> g@[gi],ei,et,cl,ci,bs
            | E is -> g,ei@[is],et,cl,ci,bs
            | L s  ->
                begin match et with
                  | None -> Stdlib.failwith "entry block has no terminator"
                  | _    ->
                      begin match cl with
                        | Some l -> g,ei,et,Some s,[],bs@[l,[],Br s]
                        | None   -> g,ei,et,Some s,[],bs
                      end
                end
            | T t  ->
                begin match et with
                  | None -> g,ei@ci,Some t,cl,[],bs
                  | _    ->
                      begin match cl with
                        | None   -> g,ei,et,None,[],bs@[gensym "tmn",[],t]
                        | Some l -> g,ei,et,None,[],bs@[l,ci,t]
                      end
                end
          end
        )
        ([],[],None,None,[],[]) s in

    begin match et with
      | None   -> Stdlib.failwith "entry block has no terminator"
      | Some t ->
          begin match cl with
            | None   -> g,((ei,t),bs)
            | Some l -> g,((ei,t),bs@[l,ci,Br l])
          end
    end
    
  let cmp_gexp (c : Ctxt.t) (e : annt_exp) : operand * Ll.llty * ginstr list =
    begin match e with
      | LitInt  i, t -> IConst i, cmp_ty t, []
      | LitFlt  f, t -> FConst f, cmp_ty t, []
      | LitBool b, t -> IConst (if b then 1L else 0L), cmp_ty t, []
      | LitChar c, t -> IConst (Int64.of_int (Char.code c)), cmp_ty t, []
      | _ -> Stdlib.failwith "bad AST: cannot have these expressions in global scope"
    end
  
  let cmp_gstmt (c : Ctxt.t) (gs : annt_gstmt) : Ctxt.t * ginstr list =
    begin match gs with
      | Module m -> Ctxt.set_current_module c m, []
      | GVDecl (id,_,t,e) ->
          let op,ellt,s = cmp_gexp c e in
          let vt =
            begin match t with
              | None   -> snd e
              | Some t -> t
            end in
          let vllt = cmp_ty vt in
          let llid = gensym id in
          Ctxt.add_binding c (id, (vt, vllt, Gid llid)),
          s @ [ GDecl (llid, vllt, op) ]
      | GFDecl (id,args,rt,b) ->
          (* allocate a stack slot for all variables *)
          let create_fstart (args : (string * ty) list) : Ctxt.t * stream =
            let c' = Ctxt.add_level c in
            List.fold_left
              (fun (c,s) (id,t) ->
                let llt,llid = cmp_ty t, gensym id in
                let ins = [ E (Alloca (llid, llt)) ; E (Store (llt, Id id, Id llid)) ] in
                Ctxt.add_binding c (id,(t,llt,Id llid)), s @ ins
              )
              (c',[]) args
          in
          let c',inits = create_fstart args in
          let gis,cfg = make_cfg (inits @ snd (cmp_block rt c' [] b)) in
          c,
          gis @ Ll.[ FDecl (
              (fun (_,_,op) -> begin match op with | Gid id -> id | _ -> Stdlib.failwith "bad create_fctxt" end) (Ctxt.get c id)
            , cmp_retty rt
            , List.map (fun (id,t) -> cmp_ty t, id) args
            , cfg
            )
          ]
    end
  
  let create_fctxt (prog : annt_program) : Ctxt.t =
    List.fold_left
      (fun c gs ->
        begin match gs with
          | Module m -> Ctxt.set_current_module c m
          | GFDecl (id,args,rt,_) ->
              let ft = TRef (TFun (List.map snd args, rt)) in
              Ctxt.add_binding c (id, (ft, cmp_ty ft, Gid (gensym id)))
          | _ -> c
        end
      )
      base_ctxt prog

  let cmp_program (prog : annt_program) : ginstr list =
    let destream : stream -> Ll.instr list =
      List.map
      (function
        | I i -> i
        | _   -> Stdlib.failwith "bad instruction stream in destream() call"
      )
    in

    let c = create_fctxt prog in
    (snd @@ List.fold_left (fun (c,res) gs -> let c',g = cmp_gstmt c gs in c',res@g) (c, []) prog) @
    [ FDecl ("main", I64, [ I64, "argc" ; Ptr (Ptr I8), "argv" ],
        let rval, strvec = gensym "mainret", gensym "strvec" in
        let strvecty = TRef (TArr (TRef TStr)) in
        let maincall, term =
          begin match Ctxt.get_from_any_module c "main" with
            | TRef (TFun ([], Void)), _, mainop ->
                [ Call (None, Void, mainop, []) ], Ret (Some (I64, IConst 0L))
            | TRef (TFun ([], Ret TInt)), _, mainop ->
                [ Call (Some rval, cmp_ty TInt, mainop, []) ], Ret (Some (I64, Id rval))
            | TRef (TFun ([TRef (TArr (TRef TStr))], Void)), _, mainop ->
                [ Call (None, Void , mainop, [ cmp_ty strvecty, Id strvec ])], Ret (Some (I64, IConst 0L))
            | TRef (TFun ([TRef (TArr (TRef TStr))], Ret TInt)), _, mainop ->
                [ Call (Some rval, cmp_ty TInt, mainop, [ cmp_ty strvecty, Id strvec ])], Ret (Some (I64, Id rval))
            | _ -> Stdlib.failwith "bad AST: bad main function"
          end in
        ([ Call (Some strvec, cmp_ty strvecty, Gid "_makestrvec", [ I64, Id "argc" ; Ptr (Ptr I8), Id "argv" ]) ] @
        maincall @ destream (removeref (cmp_ty strvecty, Id strvec)),
        term),
        []
    )]

  let cmp_to_llvm (prog : annt_program) : string =
    let cmpd = cmp_program prog in
    Printf.sprintf "target triple = \"x86_64-pc-linux-gnu\"\n\n%s" (Ll.print_llprog cmpd)

end