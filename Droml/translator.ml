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
  module Ctxt = struct

    type t = (string * (Ast.ty * Ll.llty * Ll.operand)) list list

    let empty : t = [[]]

    let get (c:t) (id:string) : Ast.ty * Ll.llty * Ll.operand =
      let found,res = List.fold_left (fun (f,r) c -> if f then true,r else if List.mem_assoc id c then true,List.assoc id c else f,r) (false,(TInt,Ll.I64,Ll.IConst 0L)) c in
      if found then res else raise Not_found
      
    let add_level (c:t) : t = [] :: c

    let top_level (c:t) = List.hd c

    let add_binding (c:t) (bnd : string * (Ast.ty * Ll.llty * Ll.operand)) : t =
      (bnd :: List.hd c) :: List.tl c

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
      | TStr        -> cmp_rty (TArr TChar) (* strings are essentially just char-arrays *)
      | TArr t      -> Struct [ I64 ; Ptr (Array (0L, cmp_ty t)) ]
      | TFun (a,rt) -> Func ((List.map cmp_ty a), cmp_retty rt)
    end
  
  and cmp_retty (t : Ast.retty) : Ll.llty =
    begin match t with
      | Ast.Void  -> Ll.Void
      | Ast.Ret t -> cmp_ty t
    end

  (* context buildup with all builtin functions (see builtins.ml) *)
  let base_ctxt =
    let builtin_bindings = List.map (fun (s,t) -> s, (t, cmp_ty t, Gid s)) builtins in
    List.fold_left Ctxt.add_binding Ctxt.empty builtin_bindings

  
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
      | Id id, t ->
          let op,llt,s = cmp_lhs c e in
          begin match deptr llt with
            | Ptr (Func _) -> op, llt, s, false (* do not dereference functions *)
            | llt' ->
                let idsym = gensym id in
                Id idsym, llt', [ I (Load (idsym, llt', op)) ], false
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
      | Null rt, t      -> Null, cmp_ty t, [], false

      | Bop (op,l,r), t -> (* no GC, as all input results are primitives *)
          (* ignore gc as all inputs are primitives *)
          let rsym = gensym "binop" in
          let (op1,llt1,s1,gc1), (op2,llt2,s2,gc2) = cmp_exp c l, cmp_exp c r in

          begin match snd l, snd r with
            | TRef TStr, TRef TStr | TRef TStr, TInt | TInt, TRef TStr ->
                let fname,rt = List.assoc (op,snd l,snd r) str_bops in
                Id rsym, cmp_ty rt,
                s1 @ s2 @ [ I (Call (Some rsym, cmp_ty rt, Gid fname, [ llt1, op1 ; llt2, op2 ]))] @
                  (if gc1 then removeref (llt1,op1) else []) @ (if gc2 then removeref (llt2,op2) else []), true
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
          let isnonrefop (op : Ast.cmpop) = op <> RefEq && op <> RefNotEq in
          let res, resstack = gensym "cmp", gensym "cmp" in
          (* ignore gc as all inputs are primitives *)
          let fop,fllt,fs,gc = cmp_exp c f in
          let fstlbl, lblfalse, lblend = gensym "cmplbl", gensym "cmpfalse", gensym "cmpend" in
          let lastop,last_t,lastlbl,lastgc,s =
            List.fold_left
              (fun (lop,lt,lbl,lgc,s) (op,rexp) ->
                (* ignore gc as all inputs are primitives *)
                let rop,rllt,rs,gc = cmp_exp c rexp in
                let (lt,lllt,lop), (rt,rllt,rop), caststream = bop_cast (lt,cmp_ty lt,lop) (snd rexp,rllt,rop) in
                let nextlbl, nextres = gensym "cmplbl", gensym "cmp" in
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
                (if lgc then removeref (lllt,lop) else []) @
                [ T (Cbr (Id nextres, nextlbl, lblfalse)) ]
              )
              (fop,snd f,fstlbl,gc,[]) rs in
          Id res, cmp_ty TBool,
          [ E (Alloca (resstack, cmp_ty TBool)) ] @ fs @ [ T (Br fstlbl) ] @ s @
          (if lastgc then removeref (cmp_ty last_t,lastop) else []) @
          [ L lastlbl ; I (Store (cmp_ty TBool, IConst 1L, Id resstack)) ; T (Br lblend) ;
            L lblfalse ; I (Store (cmp_ty TBool, IConst 0L, Id resstack)) ; T (Br lblend) ;
            L lblend ; I (Load (res, cmp_ty TBool, Id resstack)) ],
          false

      | FApp (f,a), t ->
          let getstream (_,_,s,_) = s in
          let getlltandop (op,llt,_,_) = llt, op in
          (* fgc must be = false since functions are not garbage-collectable (yet) *)
          let (fop,fllt,fs,fgc), argcs = cmp_exp c f, List.map (cmp_exp c) a in
          let arggcops = List.concat (List.map2 (fun (op,llt,_,gc) (_,t) -> if gc then removeref (llt,op) else []) argcs a) in
          let rsym = gensym "callop" in
          begin match snd f with
            | TRef (TFun (_,rt)) ->
                let llrt = cmp_retty rt in
                Id rsym, llrt,
                fs @ List.concat (List.map getstream argcs) @
                [ I (Call (Some rsym, llrt, fop, List.map getlltandop argcs)) ] @ arggcops,
                begin match t with | TRef _ -> true | _ -> false end
            | _ -> Stdlib.failwith "not a function, abort"
          end

      | Subscript (l,i), t ->
          let ptrop, pllt, s = cmp_lhs c (Subscript (l,i), t) in
          let rsym = gensym "subscript" in
          Id rsym, cmp_ty t, s @ [ I (Load (rsym, cmp_ty t, ptrop)) ], false
    end
  
  and cmp_lhs (c : Ctxt.t) (e : annt_exp) : operand * Ll.llty * stream =
    begin match e with
      | Id id, t ->
          let t,llt,op = Ctxt.get c id in
          op, Ptr llt, []

      | Subscript (b,o), t ->
          let (bop,bllt,bs,bgc), (oop,ollt,os,ogc) = cmp_exp c b, cmp_exp c o in
          let c_t, c_et = cmp_ty (snd b), cmp_ty t in
          let rsym, arr_ptr, arr_op = gensym "subscript", gensym "sub_arr_ptr", gensym "sub_arr_op" in
          Id rsym, Ptr (cmp_ty t),
          bs @ os @
          [ I (Gep (arr_ptr, c_t, bop, [ IConst 0L ; IConst 1L ]))
          ; I (Load (arr_op, Ptr (Array (0L, c_et)), Id arr_ptr))
          ; I (Gep (rsym, Ptr (Array (0L, c_et)), Id arr_op, [ IConst 0L ; oop ]))
          ] @ (if bgc then removeref (bllt,bop) else [])

      | _     -> Stdlib.failwith "lhs unimplemented"
    end
  
  (* string list is list of all variables that need to be gc'd at a return statement *)
  let rec cmp_stmt (rt : Ast.retty) (c : Ctxt.t) (refvars : (llty * operand) list) (s : annt_stmt) : Ctxt.t * stream * (llty * operand) list =
    
    let cross_cast (et : ty) (lt,lllt,lop : ty * llty * operand) : (llty * operand) * stream =
      begin match et,lt with
        | TInt,TFlt | TFlt,TInt ->
            let rsym, c_et = gensym "crosscast", cmp_ty et in
            (c_et, Id rsym), [ I (Bitcast (rsym, lllt, lop, c_et)) ]
        | _ -> (lllt,lop), []
      end
    in
    
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
              | TRef _, false -> addref (ellt,op)
              | _             -> []
            end,
          begin match vt with
            | TRef _ -> (vllt, Id llid) :: refvars
            | _      -> refvars
          end
          (* add pref and remove it from %op: cancel each other out *)
      | Assn (l,r) ->
          let (lop,lllt,ls), (rop,rllt,rs,rgc) = cmp_lhs c l, cmp_exp c r in
          let (rllt,rop),crosscaststream = cross_cast (snd l) (snd r, rllt, rop) in
          let gc_prevval =
            begin match snd l with
              | TRef _ ->
                  let gcobj, lllt' = gensym "gc_prevval", deptr lllt in
                  [ I (Load (gcobj, lllt', lop)) ] @ removeref (lllt',Id gcobj) @ (if rgc then [] else addref (rllt, rop))
              | _ -> [] (* do not attempt to gc primitives *)
            end in
          c, ls @ rs @ crosscaststream @ gc_prevval @ [ I (Store (rllt, rop, lop)) ], refvars
      | Expr (e,t) ->
          begin match e with
            | FApp (f,a) ->
                let getstream (_,_,s,_) = s in
                let getlltandop (op,llt,_,_) = llt, op in
                (* fgc = false, functions not GC'able yet *)
                let (fop,_,fs,fgc), argcs = cmp_exp c f, List.map (cmp_exp c) a in
                let arggcops = List.concat (List.map2 (fun (op,llt,_,gc) (_,t) -> if gc then maybe_removeref t (llt,op) else []) argcs a) in
                begin match snd f with
                  | TRef (TFun (_, Ret (TRef t))) ->
                      let ret_name, cd_retty = gensym "ret_ref", cmp_retty (Ret (TRef t)) in
                      c,
                      fs @ List.concat (List.map getstream argcs) @
                        [ I (Call (Some ret_name, cd_retty, fop, List.map getlltandop argcs)) ] @
                        removeref (cd_retty, Id ret_name) @ arggcops,
                      refvars
                  | TRef (TFun (_,rt)) ->
                      c,
                      fs @ List.concat (List.map getstream argcs) @
                        [ I (Call (None, cmp_retty rt, fop, List.map getlltandop argcs)) ] @ arggcops,
                      refvars
                  | _ -> Stdlib.failwith "function call doesn't work"
                end
            | _ -> Stdlib.failwith "can only compile function call as expression statement"
          end
      | If (cnd,t,nt) -> (* conditionals are bool -> primitive -> not GC'able (same for while, do-while) *)
          let cop,_,cs,_ = cmp_exp c cnd in
          let (_,s1), (_,s2) = cmp_block rt c refvars t, cmp_block rt c refvars nt in
          let lbl1, lbl2, lblend = gensym "if_lbl", gensym "if_lbl", gensym "lbl_end" in
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
                let op,lt,s,gc = cmp_exp c e in
                c, s @ (if gc then [] else maybe_addref (snd e) (lt,op)) @ free_vars @ [ T (Ret (Some (cmp_retty rt, op))) ], refvars
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
                  | _    -> g,ei,et,Some s,[],bs
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
      | GVDecl (id,_,t,e) ->
          let op,ellt,s = cmp_gexp c e in
          let vt =
            begin match t with
              | None   -> snd e
              | Some t -> t
            end in
          let vllt = cmp_ty vt in
          let llid = gensym id in
          Ctxt.add_binding c (id, (vt, Ptr vllt, Gid llid)),
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
              id
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
          | GFDecl (id,args,rt,_) ->
              let ft = TRef (TFun (List.map snd args, rt)) in
              Ctxt.add_binding c (id, (ft, cmp_ty ft, Gid id))
          | _ -> c
        end
      )
      base_ctxt prog

  let cmp_program (prog : annt_program) : ginstr list =
    snd @@ List.fold_left (fun (c,res) gs -> let c',g = cmp_gstmt c gs in c',res@g) (create_fctxt prog, []) prog

  let cmp_to_llvm (prog : annt_program) : string =
    let cmpd = cmp_program prog in
    Printf.sprintf "target triple = \"x86_64-pc-linux-gnu\"\n\n%s" (Ll.print_llprog cmpd)

end