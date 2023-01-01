open Common
open Token
open Ast
open Astannotated
open Builtins

module TypeChecker = struct

  exception TypeError of string node

  module Ctxt = struct

    (* List.hd <=> top level block *)
    type t = ((string * (ty * mutability)) list) list

    let empty : t = [[]]

    let has (c:t) (id:string) : bool =
      List.fold_left (fun b l -> if b then true else List.mem_assoc id l) false c

    let has_toplevel (c:t) (id:string) : bool = List.mem_assoc id (List.hd c)

    let get (c:t) (id:string) : ty * mutability =
      let found,res =
        List.fold_left (fun (f,r) l -> if f then true,r else if List.mem_assoc id l then true, List.assoc id l else false,r) (false,(TInt,Const)) c in
      if found then res else raise Not_found
    
    let add_level (c:t) : t = [] :: c

    let add_binding (c:t) (bnd : string*(ty*mutability)) : t =
      (bnd :: List.hd c) :: List.tl c

  end

  let rec crossp (l : 'a list list) : 'a list list = 
    let rec aux acc l1 l2 =
      begin match l1, l2 with
        | [], _ | _, [] -> acc
        | h1::t1, h2::t2 -> 
            let acc = (h1::h2)::acc in
            let acc = (aux acc t1 l2) in
            aux acc [h1] t2
      end
    in
    begin match l with
      | [] -> []
      | [l1] -> List.map (fun x -> [x]) l1
      | l1::tl ->
          let tail_product = crossp tl in
          aux [] l1 tail_product
    end
  
  let rec crossp_single (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
    begin match l1 with
      | []    -> []
      | l::ls -> List.map (fun l2e -> l,l2e) l2 @ crossp_single ls l2
    end
  
  let intersect_single (l1 : 'a list) (l2 : 'a list) : 'a list =
    List.fold_left (fun i x -> if List.mem x l2 then x::i else i) [] l1

  let intersect (l : 'a list list) : 'a list =
    begin match l with
      | []    -> []
      | l::ls -> List.fold_left intersect_single l ls
    end
  
  let rec alldistinct (l : 'a list) : bool =
    begin match l with
      | []    -> true
      | x::xs -> if List.mem x xs then false else alldistinct xs
    end
  
  let startcontext : Ctxt.t =
    List.fold_left (fun c f -> Ctxt.add_binding c (fst f, (snd f, Const))) Ctxt.empty builtins

  let uop_types : (uop * (ty * ty) list) list =
    [ Neg, [ 
        TInt, TInt
      ; TFlt, TFlt
      ]
    ; Not, [
        TBool, TBool
      ]
    ]
  
  let bop_types : (bop * ((ty * ty) * ty) list) list =
    let intop = [(TInt, TInt), TInt] in
    let fltop = [(TFlt, TFlt), TFlt] in
    let fltintop = [(TInt, TFlt), TFlt ; (TFlt, TInt), TFlt] in
    let numop = intop @ fltop @ fltintop in
    let boolop = [(TBool, TBool), TBool] in
    let numcharop = numop @ [(TInt, TChar), TChar ; (TChar, TInt), TChar] in
    [ Pow, numop
    ; Mul, numop @ [(TRef TStr, TInt), TRef TStr ; (TInt, TRef TStr), TRef TStr]
    ; Add, numcharop @ [(TRef TStr,TRef TStr), TRef TStr] ; Sub, numcharop
    ; Shl, intop ; Shr, intop ; Sha, intop
    ; Bitand, intop ; Bitxor, intop ; Bitor, intop
    ; Logand, boolop ; Logxor, boolop ; Logor, boolop
    ]
  
  (* cmp ops, all of the same type, all return bool *)
  let cmpop_types : (ty * ty) list =
    [ TInt, TInt  ; TFlt,  TFlt
    ; TInt, TFlt  ; TFlt,  TInt
    ; TChar,TChar
    ; TRef TStr, TRef TStr
    ]
  
  let rec crosstype (t1:ty) (t2:ty) : bool =
    begin match t1,t2 with
      | TInt,TFlt | TFlt,TInt -> true
      | _                     -> false
    end

  let rec subtype (t1:ty) (t2:ty) : bool =
    begin match t1,t2 with
      | TRef (TFun (a1,r1)), TRef (TFun (a2,r2)) ->
          begin match r1,r2 with
            | Void,   Void   -> true
            | Ret r1, Ret r2 -> subtype r1 r2
            | _              -> false
          end &&
            List.for_all2 subtype a2 a1
      | TNullRef t1,    TNullRef t2    -> subtype (TRef t1) (TRef t2)
      | TRef t1,        TNullRef t2    -> subtype (TRef t1) (TRef t2)
      | TRef (TArr t1), TRef (TArr t2) -> subtype t1 t2
      | t1,             t2             -> t1 = t2
    end
  
  let rec subtys (t:ty) : ty list =
    begin match t with
      | TInt | TFlt | TChar | TBool -> [t]
      | TRef TStr                   -> [TRef TStr]
      | TNullRef t                  ->
          let sbts = subtys (TRef t) in
          sbts @ (List.concat @@ List.map (fun t -> begin match t with | TRef t -> [TNullRef t] | _ -> [] end) sbts)
      | TRef (TArr t)               ->
          List.map (fun t -> TRef (TArr t)) (subtys t)
      | TRef (TFun (args,rt))       ->
          let spts = List.map suptys args in
          let rspts =
            begin match rt with
              | Void  -> [Void]
              | Ret t -> List.map (fun t -> Ret t) (subtys t) 
            end in
          let arglists = crossp spts in
          let fts = crossp_single arglists rspts in
          List.map (fun (fta,ftr) -> TRef (TFun (fta,ftr))) fts
    end
  and suptys (t:ty) : ty list =
    begin match t with
      | TInt | TFlt | TChar | TBool -> [t]
      | TRef TStr                   -> [TRef TStr ; TNullRef TStr]
      | TNullRef t                  ->
          let spts = suptys (TRef t) in
          List.concat @@ List.map (fun t -> begin match t with | TRef t -> [TNullRef t] | _ -> [] end) spts
      | TRef (TArr t)               ->
          let spts = subtys t in
          List.map (fun t -> TRef (TArr t)) spts @ List.map (fun t -> TNullRef (TArr t)) spts
      | TRef (TFun (args,rt))       ->
          let sbts = List.map subtys args in
          let rsbts =
            begin match rt with
              | Void  -> [Void]
              | Ret t -> List.map (fun t -> Ret t) (suptys t)
            end in
          let arglists = crossp sbts in
          let fts = crossp_single arglists rsbts in
          List.map (fun (fta,ftr) -> TRef (TFun (fta,ftr))) fts
    end
  
  let rec is_assignable (c:Ctxt.t) (e:exp node) : bool =
    let rec is_assignable_with_const (e:exp node) : bool =
      begin match e.t with
        | Id id           -> Ctxt.has c id
        | Subscript (b,o) -> is_assignable_with_const b
        | _               -> false
      end
    in
    begin match e.t with
      | Id id       ->
          if Ctxt.has c id then
            begin match Ctxt.get c id with
              | _,Mut -> true
              | _     -> false
            end
          else
            false
      | Subscript (b,o) -> is_assignable_with_const b
      | _               -> false
    end   
    
  let rec is_printable (t:ty) : bool =
    begin match t with
      | TInt | TFlt | TChar | TBool | TRef TStr -> true
      | TRef (TArr t) -> is_printable t
      | _ -> false
    end
  
  let rec check_exp (c:Ctxt.t) (exp_t : ty option) (e:exp node) : annt_exp =
    begin match e.t with
      | Id id ->
          if Ctxt.has c id then Id id, fst (Ctxt.get c id)
          else raise @@ TypeError (ofnode (Printf.sprintf "Variable '%s' not declared" id) e)
      | LitInt  i   -> LitInt i, TInt
      | LitFlt  f   -> LitFlt f, TFlt
      | LitChar c   -> LitChar c, TChar
      | LitBool b   -> LitBool b, TBool
      | LitStr  s   -> LitStr s, TRef TStr
      | LitArr  ls  ->
          let annt_es = List.map (check_exp c None) ls in
          let spts = List.map suptys @@ List.map snd annt_es in
          begin match intersect spts with
            | []    -> raise @@ TypeError (ofnode "Types in array must have a common supertype" e)
            | t::ts -> LitArr annt_es, TRef (TArr (List.fold_left (fun m t -> if subtype t m then t else m) t ts))
          end
      | EmptyList t -> EmptyList t.t, TRef (TArr t.t)
      | RangeList (e1,i1,i2,e2) ->
          let e1',e2' = check_exp c None e1, check_exp c None e2 in
          begin match snd e1', snd e2' with
            | TInt,TInt -> RangeList (e1',i1,i2,e2'), TRef (TArr TInt)
            | _ -> raise @@ TypeError (ofnode "Range list expressions should be of type int" e)
          end
      | ListComp (ex,vs,cnd) ->
          let avs = List.map (fun (id,e) -> id, check_exp c None e) vs in
          let c' =
            List.fold_left
            (fun c (id,(_,t)) ->
              begin match t with
                | TRef (TArr t') -> Ctxt.add_binding c (id,(t',Const))
                | _              -> raise @@ TypeError (ofnode "list comprehension variable should be in an array type" e)
              end
            )
            c avs in
          let ae, acnd = check_exp c' None ex, check_exp c' None cnd in
          ListComp (ae,avs,acnd), TRef (TArr (snd ae))
      | Ternary (cnd,e1,e2) ->
          let cnd',e1',e2' = check_exp c None cnd, check_exp c None e1, check_exp c None e2 in
          if snd cnd' = TBool then
            begin match intersect_single (suptys (snd e1')) (suptys (snd e2')) with
              | []    -> raise @@ TypeError (ofnode "types in ternary expression must have common supertype" e)
              | t::ts -> Ternary (cnd',e1',e2'), List.fold_left (fun m t -> if subtype t m then t else m) t ts
            end
          else
            raise @@ TypeError (ofnode "ternary condition must be of type bool" cnd)
      | Null    rt  -> Null rt.t, TNullRef rt.t
      | Sprintf (Sprintf, s, es) ->
          let annt_es = List.map (check_exp c None) es in
          let indexstrs = Str.full_split (Str.regexp "{\\d+}") s.t in
          let strindices = List.filter_map (function | Str.Delim s -> Some (Stdlib.int_of_string (String.sub s 1 (String.length s - 2))) | _ -> None) indexstrs in
          if List.for_all (fun i -> 0 <= i && i < List.length annt_es) strindices && List.for_all (fun (_,t) -> is_printable t) annt_es then
            Sprintf (Sprintf, s.t, annt_es), TRef TStr
          else
            raise @@ TypeError (ofnode "Something is wrong with this sprintf expression" e)
      | Sprintf (Printf, _, _) ->
          raise @@ TypeError (ofnode "printf expression returns void, cannot be used in an expression" e)
      | Uop (op,r) ->
          let opts = List.assoc op uop_types in
          let expt = check_exp c None r in
          begin match List.assoc_opt (snd expt) opts with
            | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand type %s" (List.assoc op uop_string) (print_ty (ofnode (snd expt) r))) r)
            | Some t -> Uop (op, expt), t
          end
      | Bop (op,l,r) ->
          let opts = List.assoc op bop_types in
          let lt,rt = check_exp c None l, check_exp c None r in
          begin match snd lt, snd rt with
            | TRef (TArr t1), TRef (TArr t2) ->
                begin match intersect_single (suptys t1) (suptys t2) with
                  | []    -> raise @@ TypeError (ofnode "Types in array must have a common supertype" e)
                  | t::ts -> Bop (op,lt,rt), TRef (TArr (List.fold_left (fun m t -> if subtype t m then t else m) t ts))
                end
            | _ ->
                begin match List.assoc_opt (snd lt, snd rt) opts with
                  | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand types (%s,%s)" (List.assoc op bop_string) (print_ty (ofnode (snd lt) l)) (print_ty (ofnode (snd rt) l))) e)
                  | Some t -> Bop (op, lt, rt), t
                end
          end
      | Cmps (x,xs) ->
          let first_exp = check_exp c None x in
          let _,clist =
            List.fold_left
              (fun (lt,l) (op,r) -> 
                let rt = check_exp c None r in
                begin match op with
                  | RefEq | RefNotEq ->
                      begin match lt, snd rt with
                        | TRef _, _ | _, TRef _ | TNullRef _, _ | _, TNullRef _ ->
                            if subtype lt (snd rt) || subtype (snd rt) lt then
                              snd rt, l @ [op, rt]
                            else
                              raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s only takes related reference types" (List.assoc op cmp_string)) r)
                        | _ -> raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s only takes reference types" (List.assoc op cmp_string)) r)
                      end
                  | _ ->
                      if List.mem (lt, snd rt) cmpop_types then
                        snd rt, l @ [op, rt]
                      else
                        raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s undefined for operand types (%s,%s)" (List.assoc op cmp_string) (print_ty (ofnode lt e)) (print_ty (ofnode (snd rt) r))) r)
                end
              )
              (snd first_exp, []) xs in
          Cmps (first_exp, clist), TBool
      | FApp (f,args) ->
          let argts = List.map (check_exp c None) args in
          begin match check_exp c None f with
            | f', TRef (TFun (a,rt)) ->
                let _ = if List.length a <> List.length argts then raise @@ TypeError (ofnode (Printf.sprintf "Argument list must match the length of the function argument list (type %s)" (print_ty (ofnode (TRef (TFun (a,rt))) f))) e) else () in
                let () = List.iter2
                  (fun (aexp,pt) fet ->
                    if subtype pt fet || crosstype pt fet then ()
                    else raise @@ TypeError (ofnode (Printf.sprintf "Argument type mismatch in function application: type %s vs expected type %s" (print_ty (ofnode pt e)) (print_ty (ofnode fet f))) e)  
                  )
                  argts a in
                begin match rt with
                  | Void  -> raise @@ TypeError (ofnode "Function in expression must not be of void type" f)
                  | Ret t -> FApp ((f', TRef (TFun (a,rt))), argts), t 
                end
            | t -> raise @@ TypeError (ofnode (Printf.sprintf "Type %s cannot act as a function" (print_ty (ofnode (snd t) f))) f)
          end
      | Subscript (l,r) ->
          begin match check_exp c None l, check_exp c None r with
            | (l', TRef (TArr t)), (r', TInt) -> Subscript ((l', TRef (TArr t)), (r', TInt)), t
            | (_, t),              (_, TInt)  -> raise @@ TypeError (ofnode (Printf.sprintf "Left-hand-side of [] expression is of type %s but should be of array type" (print_ty (ofnode t l))) l)
            | _,                   (_, t)     -> raise @@ TypeError (ofnode (Printf.sprintf "Right-hand-side of [] expression is of type %s but should be of int type" (print_ty (ofnode t r))) r)
          end
    end
  
  let rec check_stmt (rt : retty) (c : Ctxt.t) (s : stmt node) : annt_stmt * Ctxt.t * bool = 
    begin match s.t with
      | VDecl (id,m,t,e) ->
          if Ctxt.has_toplevel c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in this block" id) s)
          else
            let et = check_exp c None e in
            begin match t with
              | None   -> VDecl (id,m,None,et), Ctxt.add_binding c (id,(snd et,m)), false
              | Some t ->
                  if subtype (snd et) t.t || crosstype (snd et) t.t then
                    VDecl (id,m,Some t.t,et), Ctxt.add_binding c (id,(t.t,m)), false
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match: %s vs %s" (print_ty t) (print_ty (ofnode (snd et) e))) s)
            end
      | Assn (l,r) ->
          let lt,rt = check_exp c None l, check_exp c None r in
          if is_assignable c l then
            if subtype (snd rt) (snd lt) || crosstype (snd lt) (snd rt) then
              Assn (lt,rt), c, false
            else
              raise @@ TypeError (ofnode (Printf.sprintf "expression type doesn't match assignment target") r)
          else
            raise @@ TypeError (ofnode ("left-hand side expression cannot be written to") l)
      | Expr e ->
          begin match e.t with
            | Sprintf (Printf, s, es) ->
                let annt_es = List.map (check_exp c None) es in
                let indexstrs = Str.full_split (Str.regexp "{\\d+}") s.t in
                let strindices = List.filter_map (function | Str.Delim s -> Some (Stdlib.int_of_string (String.sub s 1 (String.length s - 2))) | _ -> None) indexstrs in
                if List.for_all (fun i -> 0 <= i && i < List.length annt_es) strindices && List.for_all (fun (_,t) -> is_printable t) annt_es then
                  Expr (Sprintf (Printf, s.t, annt_es), None), c, false
                else
                  raise @@ TypeError (ofnode "Something is wrong with this printf expression" e)
            | FApp (f,args) ->
                let argts = List.map (check_exp c None) args in
                begin match check_exp c None f with
                  | f', TRef (TFun (a,rt)) ->
                      let _ = if List.length a <> List.length argts then raise @@ TypeError (ofnode (Printf.sprintf "Argument list must match the length of the function argument list (type %s)" (print_ty (ofnode (TRef (TFun (a,rt))) f))) e) else () in
                      let () = List.iter2
                        (fun (aexp,pt) fet ->
                          if subtype pt fet || crosstype pt fet then ()
                          else raise @@ TypeError (ofnode (Printf.sprintf "Argument type mismatch in function application: type %s vs expected type %s" (print_ty (ofnode pt e)) (print_ty (ofnode fet f))) e)  
                        )
                        argts a in
                      Expr ((FApp ((f', TRef (TFun (a,rt))),argts), begin match rt with | Void -> None | Ret t -> Some t end)), c, false
                  | t -> raise @@ TypeError (ofnode (Printf.sprintf "Type %s cannot act as a function" (print_ty (ofnode (snd t) f))) f)
                end
            | _ -> raise @@ TypeError (ofnode "expression statements must be function calls" e)
          end
      | If (cd,t,n) ->
          let ct = check_exp c None cd in
          if snd ct = TBool then
            let (t',_,r1),(n',_,r2) = check_stmt_block rt c t, check_stmt_block rt c n in If (ct,t',n'), c, r1 && r2
          else
            raise @@ TypeError (ofnode "if condition must be of type bool" cd)
      | Denull (id,e,t,n) ->
          let e',et = check_exp c None e in
          begin match et with
            | TNullRef r ->
                let c' = Ctxt.add_level @@ Ctxt.add_binding (Ctxt.add_level c) (id, (TRef r, Const)) in
                let (t',_,r1), (n',_,r2) = check_stmt_block rt c' t, check_stmt_block rt c n in
                Denull (id,(e',et),t',n'), c, r1 && r2
            | _ -> raise @@ TypeError (ofnode "expression in checked cast must be a maybe-null reference" e)
          end
      | While (cd,b) ->
          let ct = check_exp c None cd in
          if snd ct = TBool then
            let b',_,_ = check_stmt_block rt c b in While (ct,b'), c, false
          else
            raise @@ TypeError (ofnode "while condition must be of type bool" cd)
      | DoWhile (cd,b) ->
          let ct = check_exp c None cd in
          if snd ct = TBool then
            let b',_,r = check_stmt_block rt c b in DoWhile (ct,b'), c, r
          else
            raise @@ TypeError (ofnode "do-while condition must be of type bool" cd)
      | For (id,exps,incl1,incl2,expe,b) ->
          let sty, ety = check_exp c None exps, check_exp c None expe in
          if snd sty = TInt && snd ety = TInt then
            let c' = Ctxt.add_binding (Ctxt.add_level c) (id,(TInt,Const)) in
            let b',_,_ = check_stmt_block rt c' b in For (id,sty,incl1,incl2,ety,b'), c, false
          else
            raise @@ TypeError (ofnode "for loops bounds must be of type int" s)
      | Return None ->
          if rt = Void then Return None, c, true
          else raise @@ TypeError (ofnode "cannot return without expression in non-void function" s)
      | Return (Some e) ->
          let et = check_exp c None e in
          begin match rt with
            | Void  -> raise @@ TypeError (ofnode "cannot return with expression in void function" s)
            | Ret t ->
                if subtype (snd et) t || crosstype (snd et) t then
                  Return (Some et), c, true
                else
                  raise @@ TypeError (ofnode "return type doesn't match with function return type" s)
          end
    end
  and check_stmt_block (rt : retty) (c : Ctxt.t) : stmt node list -> (annt_stmt list * Ctxt.t * bool) =
    List.fold_left (fun (b,c,r) s -> if r then raise (TypeError (ofnode "unreachable statement" s)) else let s',c',r' = check_stmt rt c s in b@[s'], c', r') ([],c,false)

  let rec check_gexp (c : Ctxt.t) (ge : exp node) : annt_exp =
    begin match ge.t with
      | LitInt  i -> LitInt i,  TInt
      | LitFlt  f -> LitFlt f,  TFlt
      | LitChar c -> LitChar c, TChar
      | LitBool b -> LitBool b, TBool
      | LitStr  s -> LitStr s,  TRef TStr
      | _         -> raise @@ TypeError (ofnode "illegal global expression" ge)
    end

  let check_gstmt (c : Ctxt.t) (gs : gstmt node) : annt_gstmt * Ctxt.t =
    begin match gs.t with
      | GVDecl (id,m,t,e) ->
          if Ctxt.has c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in global scope" id) gs)
          else
            let et = check_gexp c e in
            begin match t with
              | None   -> GVDecl(id,m,None,et), Ctxt.add_binding c (id,(snd et,m))
              | Some t ->
                  if subtype (snd et) t.t then
                    GVDecl (id,m,Some t.t,et), Ctxt.add_binding c (id,(t.t,m))
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match") gs)
            end
      | GFDecl (id,args,rt,b) ->
          if alldistinct (List.map fst args) then
            let c' = Ctxt.add_level c in
            let c'' = List.fold_left (fun c (id,t) -> Ctxt.add_binding c (id,(t.t,Const))) c' args in
            let b',_,returns = check_stmt_block rt.t c'' b in
            if returns then
              GFDecl(id, List.map (fun (s,t) -> s,t.t) args, rt.t, b'), c
            else if rt.t = Void then
              GFDecl(id, List.map (fun (s,t) -> s,t.t) args, rt.t, b' @ [ Return None ]), c
            else
              raise @@ TypeError (ofnode "Function must return" gs)
          else
            raise @@ TypeError (ofnode "Function argument names must be distinct" gs)
    end
  
  let check_gstmt_program (c:Ctxt.t) (gs : gstmt node list) : (annt_gstmt list * Ctxt.t) =
    List.fold_left (fun (l,c) gs -> let ag,c' = check_gstmt c gs in l@[ag],c') ([],c) gs

  let create_fctxt (c : Ctxt.t) (gs : gstmt node) : Ctxt.t =
    begin match gs.t with
      | GFDecl (id,args,rt,_) -> Ctxt.add_binding c (id,(TRef (TFun (List.map (fun (_,t) -> t.t) args, rt.t)), Const))
      | GVDecl _              -> c
    end
  
  let create_fctxt_program : Ctxt.t -> gstmt node list -> Ctxt.t = List.fold_left create_fctxt

  let check_program (prog : gstmt node list) : annt_gstmt list =
    let c = create_fctxt_program startcontext prog in
    let _ =
      if Ctxt.has c "main" then
        begin match Ctxt.get c "main" with
          | TRef (TFun ([], Void)),                            Const -> ()
          | TRef (TFun ([], Ret TInt)),                        Const -> ()
          | TRef (TFun ([TRef (TArr (TRef TStr))], Void)),     Const -> ()
          | TRef (TFun ([TRef (TArr (TRef TStr))], Ret TInt)), Const -> ()
          | _ ->
              let mainfunc = List.hd (List.filter
                (fun gs ->
                  begin match gs.t with
                    | Ast.GFDecl ("main", _, _, _) -> true
                    | _                            -> false
                  end
                )
                prog
              ) in
              raise @@ TypeError (ofnode "main function is of wrong type" mainfunc)
        end
      else
        raise @@ TypeError { t = "program must contain a main function" ; start = 0 ; length = 1 }
    in
    fst @@ check_gstmt_program c prog

end