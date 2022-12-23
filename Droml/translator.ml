open Common
open Ast
open Ll
open Builtins

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
      | TArr t      -> Struct [ I64 ; Ptr (cmp_ty t) ]
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
  

  let rec cmp_exp (c : Ctxt.t) (e : Ast.exp node) : operand * Ast.ty * Ll.llty * stream =

    let bop_ts : ((Ast.bop * Ast.ty * Ast.ty) * (Ast.ty * Ll.bop)) list =
      [ (Add,    TInt,  TInt ), (TInt,  Add )
      ; (Add,    TFlt,  TFlt ), (TFlt,  FAdd)
      ; (Sub,    TInt,  TInt ), (TInt,  Sub )
      ; (Sub,    TFlt,  TFlt ), (TFlt,  FSub)
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
      ] in
    
    let allocate (size : Ll.operand) (t : Ll.llty) (name : string) : stream =
      let ptrsym = gensym "malloc" in
      [ I (Call (Some ptrsym, Ptr I8, Gid "_allocate", [ I64, size ]))
      ; I (Bitcast (name, Ptr I8, Id ptrsym, t))
      ]
    in

    let addchild (from : llty * operand) (to_op : llty * operand) : stream =
      let bc1sym, bc2sym = gensym "addchild", gensym "addchild" in
      [ I (Bitcast (bc1sym, fst from, snd from, Ptr I8))
      ; I (Bitcast (bc2sym, fst to_op, snd to_op, Ptr I8))
      ; I (Call (None, Void, Gid "_addchild", [Ptr I8, Id bc1sym ; Ptr I8, Id bc2sym]))
      ]
    in

    begin match e.t with
      | Ast.Id      id ->
          let op,t,llt,s = cmp_lhs c e in
          begin match llt with
            | Ptr (Func _) -> op, t, llt, s (* do not dereference functions *)
            | llt' ->
                let idsym = gensym id in
                Id idsym, t, llt', Ll.[ I (Load (idsym, llt', op)) ]
          end
      | Ast.LitInt  i  -> Ll.IConst i, Ast.TInt, cmp_ty Ast.TInt, []
      | Ast.LitFlt  f  -> Ll.FConst f, Ast.TFlt, cmp_ty Ast.TFlt, []
      | Ast.LitChar c  -> Ll.IConst (Int64.of_int @@ Char.code c), Ast.TChar, cmp_ty Ast.TChar, []
      | Ast.LitBool b  -> Ll.IConst (if b then 1L else 0L), Ast.TBool, cmp_ty Ast.TBool, []
      | Ast.LitStr  s  ->
          let strlen = Int64.of_int (String.length s + 1) in
          let str_obj_ty, str_ty = cmp_ty (TRef TStr), Ptr I8 in
          let rsym, strsym = gensym "str", gensym "str" in
          let gsym, gsym_t = gensym "gstr", Array (strlen, I8) in
          let lsym = gensym "lstr" in
          let size_ptr, str_ptr = gensym "arrsize", gensym "arrdata" in
          let alloc_obj = allocate (IConst 2L) str_obj_ty rsym in
          let alloc_str = allocate (IConst strlen) str_ty strsym in

          Id rsym, TRef TStr, str_obj_ty,

          (* allocate memory space for string structure and char array *)
          alloc_obj @ alloc_str @        
          (* add GC child from string structure to char array *)
          addchild (str_obj_ty, Id rsym) (str_ty, Id strsym) @
            (* create global string symbol *)
          [ G (GDecl (gsym, gsym_t, Str s))
            (* cast global char array to char pointer *)
          ; I (Bitcast (lsym, Ptr gsym_t, Gid gsym, Ptr I8))
            (* size_ptr points to size field in string structure *)
          ; I (Gep (size_ptr, str_obj_ty, Id rsym, [ IConst 0L; IConst 0L ]))
            (* str_ptr points to string field in string structure *)
          ; I (Gep (str_ptr, str_obj_ty, Id rsym, [ IConst 0L; IConst 1L ]))
            (* copy char array into string symbol *)
          ; I (Call (None, Void, Gid "_memcpy", [str_ty, Id lsym ; str_ty, Id strsym ; I64, IConst strlen]))
            (* store that pointer in the string pointer in the char structure *)
          ; I (Store (str_ty, Id strsym, Id str_ptr))
            (* store the size in the size field in the string structure *)
          ; I (Store (I64, IConst strlen, Id size_ptr))
            (* remove GC reference to child string *)
          ; I (Call (None, Void, Gid "_removeref", [str_ty, Id strsym]))
          ]
      | Ast.Bop     (op,l,r) ->
          let (op1,t1,llt1,s1), (op2,t2,llt2,s2) = cmp_exp c l, cmp_exp c r in
          let rsym = gensym "binop" in
          begin match op with
            | Pow ->
                let rt,fname = List.assoc (t1,t2) pow_ts in
                Id rsym, rt, cmp_ty rt, s1 @ s2 @ [ I (Call (Some rsym, cmp_ty rt, Gid fname, [ llt1, op1; llt2, op2 ])) ]
            | Logand | Logor ->
                let shortcircuiteval, shortcircuitstore =
                  if op = Logand then 0L, 0L else 1L, 1L in
                let cmpres = gensym "and" in
                let shortcircuit, evalboth, logend = gensym "shortcircuit", gensym "evalboth", gensym "logend" in
                let resstack = gensym "logstack" in
                Id rsym, TBool, cmp_ty TBool,
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
                ]
            | _ ->
                let rt,llop = List.assoc (op,t1,t2) bop_ts in
                Id rsym, rt, cmp_ty rt, s1 @ s2 @ [ I (Binop (rsym, llop, cmp_ty rt, op1, op2))]
          end
      | Ast.Uop (op,e) ->
          let (eop,t,llt,s) = cmp_exp c e in
          let rsym = gensym "uop" in
          let rt,llop,argop = List.assoc (op,t) uop_ts in
          Id rsym, rt, cmp_ty rt, s @ [ I (Binop (rsym, llop, cmp_ty rt, argop, eop)) ]
      | Ast.Cmps (f,rs) ->
          let res, resstack = gensym "cmp", gensym "cmp" in
          let fop,ft,_,fs = cmp_exp c f in
          let fstlbl, lblfalse, lblend = gensym "cmplbl", gensym "cmpfalse", gensym "cmpend" in
          let _,_,lastlbl,s =
            List.fold_left
              (fun (lop,lt,lbl,s) (op,rexp) ->
                let rop,rt,rllt,rs = cmp_exp c rexp in
                let nextlbl, nextres = gensym "cmplbl", gensym "cmp" in
                let cmpty =
                  begin match lt with
                    | TFlt -> FCmp
                    | _    -> ICmp (* ints and chars, faulty types removed by typechecker *)
                  end in
                let cmpop = List.assoc op cmpop_to_ll in
                rop, rt, nextlbl,
                s @ [ L lbl ] @ rs @
                [ I (Cmp (nextres, cmpty, cmpop, rllt, lop, rop)) ; T (Cbr (Id nextres, nextlbl, lblfalse)) ]
              )
              (fop,ft,fstlbl,[]) rs in
          Id res, TBool, cmp_ty TBool,
          [ E (Alloca (resstack, cmp_ty TBool)) ] @ fs @ [ T (Br fstlbl) ] @ s @
          [ L lastlbl ; I (Store (cmp_ty TBool, IConst 1L, Id resstack)) ; T (Br lblend) ;
            L lblfalse ; I (Store (cmp_ty TBool, IConst 0L, Id resstack)) ; T (Br lblend) ;
            L lblend ; I (Load (res, cmp_ty TBool, Id resstack)) ]
      | Ast.FApp (f,a) ->
          let getstream (_,_,_,s) = s in
          let getlltandop (op,_,llt,_) = llt, op in
          let (fop,ft,fllt,fs), argcs = cmp_exp c f, List.map (cmp_exp c) a in
          let rsym = gensym "callop" in
          begin match ft with
            | TRef (TFun (_,rt)) ->
                let llrt = cmp_retty rt in
                let drt =
                  begin match rt with
                    | Ret t -> t
                    | _     -> Stdlib.failwith "bad AST: cannot call void function in expression"
                  end in
                Id rsym, drt, llrt,
                fs @ List.concat (List.map getstream argcs) @
                [ I (Call (Some rsym, llrt, fop, List.map getlltandop argcs)) ]
            | _ -> Stdlib.failwith "not a function, abort"
          end
      | _              -> Stdlib.failwith "expression unimplemented"
    end
  
  and cmp_lhs (c : Ctxt.t) (e : Ast.exp node) : operand * Ast.ty * Ll.llty * stream =
    begin match e.t with
      | Id id ->
          let t,llt,op = Ctxt.get c id in
          op, t, llt, []
      | _     -> Stdlib.failwith "lhs unimplemented"
    end
  
  let rec cmp_stmt (rt : Ast.retty) (c : Ctxt.t) (s : Ast.stmt node) : Ctxt.t * stream =
    begin match s.t with
      | VDecl (id, _, t, e) ->
          let op,et,ellt,s = cmp_exp c e in
          let vt =
            begin match t with
              | None   -> et
              | Some t -> t.t
            end in
          let vllt = cmp_ty vt in
          let llid = gensym id in
          (* alloca and store expression result -> does this ever need a bitcast from ellt to vllt? *)
          (* store variables as pointers in context *)
          Ctxt.add_binding c (id, (vt, vllt, Id llid)),
          s @ [ E (Alloca (llid, vllt)) ; I (Store (ellt, op, Id llid)) ]
      | Assn (l,r) ->
          let (lop,lt,lllt,ls), (rop,rt,rllt,rs) = cmp_lhs c l, cmp_exp c r in
          c, ls @ rs @ [ I (Store (rllt, rop, lop)) ]
      | Expr e ->
          begin match e.t with
            | FApp (f,a) ->
                let getstream (_,_,_,s) = s in
                let getlltandop (op,_,llt,_) = llt, op in
                let (fop,ft,_,fs), argcs = cmp_exp c f, List.map (cmp_exp c) a in
                begin match ft with
                  | TRef (TFun (_,rt)) ->
                      c,
                        fs @ List.concat (List.map getstream argcs) @
                        [ I (Call (None, cmp_retty rt, fop, List.map getlltandop argcs)) ]
                  | _ -> Stdlib.failwith "function call doesn't work"
                end
            | _ -> let _,_,_,s = cmp_exp c e in c, s
          end
      | If (cnd,t,nt) ->
          let cop,_,_,cs = cmp_exp c cnd in
          let (_,s1), (_,s2) = cmp_block rt c t, cmp_block rt c nt in
          let lbl1, lbl2, lblend = gensym "if_lbl", gensym "if_lbl", gensym "lbl_end" in
          c, cs @ [ T (Cbr (cop, lbl1, lbl2)) ; L lbl1 ] @ s1 @ [ T (Br lblend) ; L lbl2 ] @ s2 @ [ T (Br lblend) ; L lblend ]
      | While (cnd,b) ->
          let cop,_,_,cs = cmp_exp c cnd in
          let _,s = cmp_block rt c b in
          let lblstart, lblbody, lblend = gensym "while_lbl", gensym "while_lbl", gensym "while_lbl" in
          c, [ T (Br lblstart) ; L lblstart ] @ cs @ [ T (Cbr (cop, lblbody, lblend)) ; L lblbody ] @ s @ [ T (Br lblstart); L lblend ]
      | DoWhile (cnd,b) ->
          let cop,_,_,cs = cmp_exp c cnd in
          let _,s = cmp_block rt c b in
          let lblstart, lblend = gensym "dowhile_lbl", gensym "dowhile_lbl" in
          c, [ T (Br lblstart) ; L lblstart ] @ s @ cs @ [ T (Cbr (cop, lblstart, lblend)) ; L lblend ]
      | For (id,s,incl,e,b) ->
          let (sop,_,_,ss), (eop,_,_,es) = cmp_exp c s, cmp_exp c e in
          let idsym, cmpres, cmp_var = gensym id, gensym "for_cmp", gensym "for_cmp" in
          let i_var, i_inc_var = gensym "for_var", gensym "for_var" in
          let lblstart, lblbody, lblend = gensym "for_lbl", gensym "for_lbl", gensym "for_end" in
          let c' = Ctxt.add_binding c (id, (TInt, cmp_ty TInt, Id idsym)) in
          let _,bs = cmp_block rt c' b in
          c,
          ss @ es @
          [ E (Alloca (idsym, I64)) ; I (Store (I64, sop, Id idsym)) ; T (Br lblstart)
          ; L lblstart ; I (Load (cmp_var, I64, Id idsym))
          ; I (Cmp (cmpres, ICmp, (if incl = Incl then LessEq else Less), I64, Id cmp_var, eop))
          ; T (Cbr (Id cmpres, lblbody, lblend))
          ; L lblbody
          ] @ bs @ [ I (Load (i_var, I64, Id idsym)) ; I (Binop (i_inc_var, Add, I64, Id i_var, IConst 1L)) ; I (Store (I64, Id i_inc_var, Id idsym)) ; T (Br lblstart) ; L lblend ]
      | Return None     -> c, [ T (Ll.Ret None) ]
      | Return (Some e) ->
          (* does this need a bitcast? *)
          let op,t,lt,s = cmp_exp c e in c, s @ [ T (Ll.Ret (Some (cmp_retty rt,op)))]
    end
  and cmp_block (rt : Ast.retty) (c : Ctxt.t) (b : Ast.stmt node list) : Ctxt.t * stream =
    List.fold_left (fun (c,s) st -> let c',sta = cmp_stmt rt c st in c',s@sta) (c,[]) b
  
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
    
  let cmp_gexp (c : Ctxt.t) (e : Ast.exp node) : operand * Ast.ty * Ll.llty * ginstr list =
    begin match e.t with
      | LitInt  i -> IConst i, TInt, cmp_ty TInt, []
      | LitFlt  f -> FConst f, TFlt, cmp_ty TFlt, []
      | LitBool b -> IConst (if b then 1L else 0L), TBool, cmp_ty TBool, []
      | LitChar c -> IConst (Int64.of_int (Char.code c)), TChar, cmp_ty TChar, []
      | _ -> Stdlib.failwith "bad AST: cannot have these expressions in global scope"
    end
  
  let cmp_gstmt (c : Ctxt.t) (gs : Ast.gstmt node) : Ctxt.t * ginstr list =
    begin match gs.t with
      | GVDecl (id,_,t,e) ->
          let op,et,ellt,s = cmp_gexp c e in
          let vt =
            begin match t with
              | None   -> et
              | Some t -> t.t
            end in
          let vllt = cmp_ty vt in
          let llid = gensym id in
          Ctxt.add_binding c (id, (vt, Ptr vllt, Gid llid)),
          s @ [ GDecl (llid, vllt, op) ]
      | GFDecl (id,args,rt,b) ->
          (* allocate a stack slot for all variables *)
          let create_fstart (args : (string * ty node) list) : Ctxt.t * stream =
            let c' = Ctxt.add_level c in
            List.fold_left
              (fun (c,s) (id,t) ->
                let llt,llid = cmp_ty t.t, gensym id in
                let ins = [ E (Alloca (llid, llt)) ; E (Store (llt, Id id, Id llid)) ] in
                Ctxt.add_binding c (id,(t.t,llt,Id llid)), s @ ins
              )
              (c',[]) args
          in
          let c',inits = create_fstart args in
          let gis,cfg = make_cfg (inits @ snd (cmp_block rt.t c' b)) in
          c,
          gis @ Ll.[ FDecl (
              id
            , cmp_retty rt.t
            , List.map (fun (id,t) -> cmp_ty t.t, id) args
            , cfg
            )
          ]
    end
  
  let create_fctxt (prog : Ast.program) : Ctxt.t =
    List.fold_left
      (fun c gs ->
        begin match gs.t with
          | GFDecl (id,args,rt,_) ->
              let ft = TRef (TFun (List.map (fun (_,t) -> t.t) args, rt.t)) in
              Ctxt.add_binding c (id, (ft, cmp_ty ft, Gid id))
          | _ -> c
        end
      )
      base_ctxt prog

  let cmp_program (prog : Ast.program) : ginstr list =
    snd @@ List.fold_left (fun (c,res) gs -> let c',g = cmp_gstmt c gs in c',res@g) (create_fctxt prog, []) prog

  let cmp_to_llvm (prog : Ast.program) : string =
    let cmpd = cmp_program prog in
    Printf.sprintf "target triple = \"x86_64-pc-linux-gnu\"\n\n%s" (Ll.print_llprog cmpd)

end