open Common
open Token
open Ast
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
    let numop = intop @ fltop in
    let boolop = [(TBool, TBool), TBool] in
    let numcharop = numop @ [(TInt, TChar), TChar ; (TChar, TInt), TChar] in
    [ Pow, numop
    ; Mul, numop
    ; Add, numcharop ; Sub, numcharop
    ; Shl, intop ; Shr, intop ; Sha, intop
    ; Bitand, intop ; Bitxor, intop ; Bitor, intop
    ; Logand, boolop ; Logxor, boolop ; Logor, boolop
    ]
  
  (* cmp ops, all of the same type, all return bool *)
  let cmpop_types : (ty * ty) list =
    [ TInt, TInt ; TFlt, TFlt ; TChar, TChar ]

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
  
  let is_assignable (c:Ctxt.t) (e:exp node) : bool =
    begin match e.t with
      | Id id       ->
          if Ctxt.has c id then
            begin match Ctxt.get c id with
              | _,Mut -> true
              | _     -> false
            end
          else
            false
      | Subscript _ -> true
      | _           -> false
    end
  
  let rec check_exp (c:Ctxt.t) (e:exp node) : ty =
    begin match e.t with
      | Id id ->
          if Ctxt.has c id then fst (Ctxt.get c id)
          else raise @@ TypeError (ofnode "Variable not declared" e)
      | LitInt  _   -> TInt
      | LitFlt  _   -> TFlt
      | LitChar _   -> TChar
      | LitBool _   -> TBool
      | LitStr  _   -> TRef TStr
      | LitArr  ls  ->
          let types = List.map (check_exp c) ls in
          let spts = List.map suptys types in
          begin match intersect spts with
            | []    -> raise @@ TypeError (ofnode "Types in array must have a common supertype" e)
            | t::ts -> TRef (TArr (List.fold_left (fun m t -> if subtype t m then t else m) t ts))
          end
      | EmptyList t -> TRef (TArr t.t)
      | Null    rt  -> TNullRef rt.t
      | Uop (op,r) ->
          let opts = List.assoc op uop_types in
          let expt = check_exp c r in
          begin match List.assoc_opt expt opts with
            | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand type %s" (List.assoc op uop_string) (print_ty (ofnode expt r))) r)
            | Some t -> t
          end
      | Bop (op,l,r) ->
          let opts = List.assoc op bop_types in
          let lt,rt = check_exp c l, check_exp c r in
          begin match List.assoc_opt (lt,rt) opts with
            | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand types (%s,%s)" (List.assoc op bop_string) (print_ty (ofnode lt l)) (print_ty (ofnode rt l))) e)
            | Some t -> t
          end
      | Cmps (x,xs) ->
          let _ =
            List.fold_left
              (fun lt r -> 
                let rt = check_exp c (snd r) in
                if List.mem (lt,rt) cmpop_types then rt
                else raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s undefined for operand types (%s,%s)" (List.assoc (fst r) cmp_string) (print_ty (ofnode lt e)) (print_ty (ofnode rt (snd r)))) (snd r))
              )
              (check_exp c x) xs in
          TBool
      | FApp (f,args) ->
          let argts = List.map (check_exp c) args in
          begin match check_exp c f with
            | TRef (TFun (a,rt)) ->
                let _ = if List.length a <> List.length argts then raise @@ TypeError (ofnode "Argument list length mismatch" e) else () in
                if List.for_all2 subtype argts a then
                  begin match rt with
                    | Void  -> raise @@ TypeError (ofnode "Function in expression must not be of void type" f)
                    | Ret t -> t 
                  end
                else
                  raise @@ TypeError (ofnode (Printf.sprintf "Function argument type mismatch(es)") e)
            | t -> raise @@ TypeError (ofnode (Printf.sprintf "Type %s cannot act as a function" (print_ty (ofnode t f))) f)
          end
      | Subscript (l,r) ->
          begin match check_exp c l, check_exp c r with
            | TRef (TArr t), TInt -> t
            | t,             TInt -> raise @@ TypeError (ofnode (Printf.sprintf "Left-hand-side of [] expression is of type %s but should be of array type" (print_ty (ofnode t l))) l)
            | _,             t    -> raise @@ TypeError (ofnode (Printf.sprintf "Right-hand-side of [] expression is of type %s but should be of int type" (print_ty (ofnode t r))) r)
          end
    end
  
  let rec check_stmt (rt : retty) (c : Ctxt.t) (s : stmt node) : Ctxt.t * bool = 
    begin match s.t with
      | VDecl (id,m,t,e) ->
          if Ctxt.has_toplevel c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in this block" id) s)
          else
            let et = check_exp c e in
            begin match t with
              | None   -> Ctxt.add_binding c (id,(et,m)), false
              | Some t ->
                  if subtype et t.t then
                    Ctxt.add_binding c (id,(t.t,m)), false
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match") s)
            end
      | Assn (l,r)        ->
          let lt,rt = check_exp c l, check_exp c r in
          if is_assignable c l then
            if subtype lt rt then
              c, false
            else
              raise @@ TypeError (ofnode (Printf.sprintf "expression type doesn't match assignment target") r)
          else
            raise @@ TypeError (ofnode ("left-hand side expression cannot be written to") l)
      | Expr e           ->
          begin match e.t with
            | FApp (f,args) ->
                let argts = List.map (check_exp c) args in
                begin match check_exp c f with
                  | TRef (TFun (a,rt)) ->
                      let _ = if List.length a <> List.length argts then raise @@ TypeError (ofnode "Argument list length mismatch" e) else () in
                      if List.for_all2 subtype argts a then
                        c, false
                      else
                        raise @@ TypeError (ofnode (Printf.sprintf "Function argument type mismatch(es)") e)
                  | t -> raise @@ TypeError (ofnode (Printf.sprintf "Type %s cannot act as a function" (print_ty (ofnode t f))) f)
                end
            | _ -> raise @@ TypeError (ofnode "expression statements must be function calls" e)
          end
      | If (cd,t,n)       ->
          let ct = check_exp c cd in
          if ct = TBool then
            let (_,r1),(_,r2) = check_stmt_block rt c t, check_stmt_block rt c n in c, r1 && r2
          else
            raise @@ TypeError (ofnode "if condition must be of type bool" cd)
      | While (cd,b)      ->
          let ct = check_exp c cd in
          if ct = TBool then
            let _ = check_stmt_block rt c b in c, false
          else
            raise @@ TypeError (ofnode "while condition must be of type bool" cd)
      | DoWhile (cd,b)    ->
          let ct = check_exp c cd in
          if ct = TBool then
            let _,r = check_stmt_block rt c b in c, r
          else
            raise @@ TypeError (ofnode "do-while condition must be of type bool" cd)
      | Return None      ->
          if rt = Void then c, true
          else raise @@ TypeError (ofnode "cannot return without expression in non-void function" s)
      | Return (Some e)  ->
          let et = check_exp c e in
          begin match rt with
            | Void  -> raise @@ TypeError (ofnode "cannot return with expression in void function" s)
            | Ret t ->
                if subtype et t then
                  c, true
                else
                  raise @@ TypeError (ofnode "return type doesn't match with function return type" s)
          end
    end
  and check_stmt_block (rt : retty) (c : Ctxt.t) : stmt node list -> (Ctxt.t * bool) =
    List.fold_left (fun (c,r) s -> if r then raise (TypeError (ofnode "unreachable statement" s)) else check_stmt rt c s) (c,false)

  let rec check_gexp (c : Ctxt.t) (ge : exp node) : ty =
    begin match ge.t with
      | LitInt  _ -> TInt
      | LitFlt  _ -> TFlt
      | LitChar _ -> TChar
      | LitBool _ -> TBool
      | LitStr  _ -> TRef TStr
      | _         -> raise @@ TypeError (ofnode "illegal global expression" ge)
    end

  let check_gstmt (c : Ctxt.t) (gs : gstmt node) : Ctxt.t =
    begin match gs.t with
      | GVDecl (id,m,t,e) ->
          if Ctxt.has c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in this block" id) gs)
          else
            let et = check_gexp c e in
            begin match t with
              | None   -> Ctxt.add_binding c (id,(et,m))
              | Some t ->
                  if subtype et t.t then
                    Ctxt.add_binding c (id,(t.t,m))
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match") gs)
            end
      | GFDecl (id,args,rt,b) ->
          if alldistinct (List.map fst args) then
            let c' = Ctxt.add_level c in
            let c'' = List.fold_left (fun c (id,t) -> Ctxt.add_binding c (id,(t.t,Const))) c' args in
            let _,returns = check_stmt_block rt.t c'' b in
            if returns then c
            else raise @@ TypeError (ofnode "Function must return" gs)
          else
            raise @@ TypeError (ofnode "Function argument names must be distinct" gs)
    end
  
  let check_gstmt_program : Ctxt.t -> gstmt node list -> Ctxt.t = List.fold_left check_gstmt

  let create_fctxt (c : Ctxt.t) (gs : gstmt node) : Ctxt.t =
    begin match gs.t with
      | GFDecl (id,args,rt,_) -> Ctxt.add_binding c (id,(TRef (TFun (List.map (fun (_,t) -> t.t) args, rt.t)), Const))
      | GVDecl _              -> c
    end
  
  let create_fctxt_program : Ctxt.t -> gstmt node list -> Ctxt.t = List.fold_left create_fctxt

  let check_program (prog : gstmt node list) : unit =
    let c = create_fctxt_program startcontext prog in
    let _ = check_gstmt_program c prog in
    ()

end