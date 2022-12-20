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
      | Ast.TInt                     -> Ll.I64
      | Ast.TFlt                     -> Ll.Double
      | Ast.TChar                    -> Ll.I8
      | Ast.TBool                    -> Ll.I1
      | Ast.TRef (Ast.TFun (args,Ast.Void)) ->
          Ll.Ptr (Ll.Func (List.map cmp_ty args, Ll.Void))
      | Ast.TRef (Ast.TFun (args,Ast.Ret t)) ->
          Ll.Ptr (Ll.Func (List.map cmp_ty args, cmp_ty t))
      | Ast.TRef t | Ast.TNullRef t  -> Stdlib.failwith "references unimplemented"
    end
  
  let cmp_retty (t : Ast.retty) : Ll.llty =
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
      [ (TInt, TInt), (TInt, "pow_ii")
      ; (TFlt, TFlt), (TFlt, "pow_ff")
      ] in

    begin match e.t with
      | Ast.Id      id ->
          let op,t,llt,s = cmp_lhs c e in
          begin match llt with
            | Ptr (Func _) -> op, t, llt, s (* do not dereference functions *)
            | Ptr llt' ->
                let idsym = gensym id in
                Id idsym, t, llt', Ll.[ I (Load (idsym, llt', op)) ]
            | _ -> Stdlib.failwith "bad cmp_lhs"
          end
      | Ast.LitInt  i  -> Ll.IConst i, Ast.TInt, cmp_ty Ast.TInt, []
      | Ast.LitFlt  f  -> Ll.FConst f, Ast.TFlt, cmp_ty Ast.TFlt, []
      | Ast.LitChar c  -> Ll.IConst (Int64.of_int @@ Char.code c), Ast.TChar, cmp_ty Ast.TChar, []
      | Ast.LitBool b  -> Ll.IConst (if b then 1L else 0L), Ast.TBool, cmp_ty Ast.TBool, []
      | Ast.Bop     (op,l,r) ->
          let (op1,t1,llt1,s1), (op2,t2,llt2,s2) = cmp_exp c l, cmp_exp c r in
          let rsym = gensym "binop" in
          begin match op with
            | Pow ->
                let rt,fname = List.assoc (t1,t2) pow_ts in
                Id rsym, rt, cmp_ty rt, s1 @ s2 @ [ I (Call (Some rsym, cmp_ty rt, Gid fname, [ llt1, op1; llt2, op2 ])) ]
            | Logand -> Stdlib.failwith "logical and (short circuit eval) unimplemented"
            | Logor  -> Stdlib.failwith "logical or (short circuit eval) unimplemented"
            | _ ->
                let rt,llop = List.assoc (op,t1,t2) bop_ts in
                Id rsym, rt, cmp_ty rt, s1 @ s2 @ [ I (Binop (rsym, llop, cmp_ty rt, op1, op2))]
          end
      | Ast.Uop (op,e) ->
          let (eop,t,llt,s) = cmp_exp c e in
          let rsym = gensym "uop" in
          let rt,llop,argop = List.assoc (op,t) uop_ts in
          Id rsym, rt, cmp_ty rt, s @ [ I (Binop (rsym, llop, cmp_ty rt, argop, eop)) ]
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
          Ctxt.add_binding c (id, (vt, Ptr vllt, Id llid)),
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
      | Return None     -> c, [ T (Ll.Ret None) ]
      | Return (Some e) ->
          (* does this need a bitcast? *)
          let op,t,lt,s = cmp_exp c e in c, s @ [ T (Ll.Ret (Some (cmp_retty rt,op)))]
    end
  and cmp_block (rt : Ast.retty) (c : Ctxt.t) (b : Ast.stmt node list) : Ctxt.t * stream =
    List.fold_left (fun (c,s) st -> let c',sta = cmp_stmt rt c st in c',s@sta) (c,[]) b
  
  let make_cfg (s : stream) : ginstr list * (Ll.firstblock * Ll.block list) =
    (*
     * Global Instructions:        ginstr list
     * Entry Block Instructions:   instr list
     * Entry Block Terminator:     term option
     * Current Block Label:        string option
     * Current Block Instructions: instr list
     * Block List:                 block list
     *)
    let g,ei,et,cbl,cbi,bs =
      List.fold_left
        ( fun (g,ei,et,cbl,cbi,bs) i ->
            begin match i with
              | I is -> if et = None then g,ei@[is],et,cbl,cbi,bs else g,ei,et,cbl,cbi@[is],bs
              | E is -> g,ei@[is],et,cbl,cbi,bs
              | L s  -> if et = None || List.length cbi > 0 then
                          Stdlib.failwith "bad place for a label"
                        else
                          g,ei,et,Some s,[],bs
              | T t  -> if et = None then g,ei,Some t,cbl,cbi,bs
                        else
                          begin match cbl with
                            | None   -> g,ei,et,None,[],bs (* multiple terminators: ignore the following ones -> easier implementation of while loops*)
                            | Some l -> g,ei,et,None,[],bs@[l,cbi,t]
                          end
              | G gi -> g@[gi],ei,et,cbl,cbi,bs
            end
        )
        ([],[],None,None,[],[]) s in
    (* et is not None (need >= 1 term); cbl is None, cbi is empty *)
    if true || cbl = None && List.length cbi = 0 then
      begin match et with
        | None   -> Stdlib.failwith "bad cfg"
        | Some t -> g,((ei,t),bs)
      end
    else
      Stdlib.failwith "bad cfg"
  
  let cmp_gstmt (c : Ctxt.t) (gs : Ast.gstmt node) : Ctxt.t * ginstr list =
    begin match gs.t with
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
      | _ -> Stdlib.failwith "global statements unimplemented"
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