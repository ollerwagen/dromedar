open Ast
open Common
open Typerelations

module TemplateResolver = struct

  let rec is_templated : ty -> bool =
    begin function
      | TTempl _ -> true
      | TRef (TArr t) | TNullRef (TArr t) -> is_templated t
      | TRef (TFun (a,rt)) | TNullRef (TFun (a,rt)) ->
          if List.exists is_templated a then true
          else
            begin match rt with
              | Void   -> false
              | Ret rt -> is_templated rt
            end
      | _ -> false (* named types, primitive types *)
    end

  (* in ty*ty list: first provided, then expected (templated?) types *)
  let resolve_templates (ts : (ty * ty) list) : (string * ty) list =
    let rec match_template (resolveds : (string * ty) list) (contract : bool) (pt : ty) (et : ty) : (string * ty) list option =
      let find_extreme f t1 t2 cmp =
        (* Debug Output *)
          let nn = {t=0;start=0;length=0} in
          let () = Printf.printf "intersect (%s) and (%s) extensions: [%s] resp. [%s]\n"
            (print_ty (ofnode t1 nn))
            (print_ty (ofnode t2 nn))
            (String.concat ", " @@ List.map (fun t -> print_ty (ofnode t nn)) (f t1))
            (String.concat ", " @@ List.map (fun t -> print_ty (ofnode t nn)) (f t2))
            in
        
        begin match intersect [f t1 ; f t2] with
          | [] -> None
          | l  -> Some (List.fold_left (fun c e -> if cmp c e then c else e) (List.hd l) (List.tl l))
        end
      in
      let suptype t1 t2 = not @@ subtype t1 t2 in
      
      if not (is_templated et) then
        Some (resolveds)
      else
        begin match pt,et,contract with
            (* t?, <a> *)
          | TNullRef rty, TTempl (true, id), true ->
              match_template resolveds contract (TRef rty) (TTempl (true,id))
            (* t, <a> *)
          | TInt, TTempl (false,_), _ | TFlt, TTempl (false,_), _ | TChar, TTempl (false,_), _ | TBool, TTempl (false,_), _ -> None
          | ty, TTempl (true,id), _ ->
              begin match List.assoc_opt id resolveds with
                | None     -> Some ((id,ty) :: resolveds)
                | Some tty ->
                    if (not contract && subtype tty ty) || (contract && subtype ty tty) then
                      let resolveds' = List.remove_assoc id resolveds in
                      Some ((id,ty) :: resolveds')
                    else if (not contract && subtype ty tty) || (contract && subtype tty ty) then
                      Some resolveds
                    else
                      let jointype =
                        if not contract then
                          find_extreme suptys ty tty subtype
                        else
                          find_extreme subtys ty tty suptype
                        in
                      begin match jointype with
                        | None -> None
                        | Some t -> Some ((id,t) :: (List.remove_assoc id resolveds))
                      end
              end
            (* t?, <a>? *)
          | TNullRef rty, TTempl (false,id), _ ->
              match_template resolveds contract (TRef rty) (TTempl (true,id))
            (* t, <a>? *)
          | TRef rty, TTempl (false, id), false ->
              match_template resolveds contract (TRef rty) (TTempl (true,id))
          | TNullRef pt, TNullRef et, _ ->
              match_template resolveds contract (TRef pt) (TRef et)
          | TRef (TArr pt), TRef (TArr et), _ | TRef (TArr pt), TNullRef (TArr et), false | TNullRef (TArr pt), TRef (TArr et), true ->
              match_template resolveds contract pt et
          | TRef (TFun (pa,prt)), TRef (TFun (ea,ert)), _ ->
              if List.length pa <> List.length ea then
                None
              else
                let rs' = List.fold_left2 (fun rs pt et -> match rs with | None -> None | Some rs -> match_template rs (not contract) pt et) (Some resolveds) pa ea in
                begin match prt, ert, rs' with
                  | Void, Void, _ -> rs'
                  | Ret prt, Ret ert, Some rs ->
                      match_template rs contract prt ert
                  | _ -> None 
                end
          | _ -> Stdlib.failwith "bad types in template resolution"
        end
    in

    let rec aux (res : (string * ty) list) (args : (ty * ty) list) : (string * ty) list =
      begin match args with
        | []               -> res
        | (pt,et) :: args' ->
            if not (is_templated et) then aux res args'
            else
              begin match match_template res false pt et with
                | None      -> [] (* as the function is guaranteed to be templates, no type bindings is equivalent to an error *)
                | Some res' -> aux res' args'
              end
      end
    in

    let resolve_solution (ts : (string * ty) list) : ty list -> ty list =
      let rec aux (t:ty) : ty =
        begin match t with
          | TInt | TFlt | TChar | TBool | TRef TStr -> t
          | TNullRef t ->
              begin match aux (TRef t) with
                | TRef t -> TNullRef t
                | _      -> raise (Invalid_argument "")
              end
          | TRef (TArr t)          -> TRef (TArr (aux t))
          | TRef (TFun (a,Void))   -> TRef (TFun (List.map aux a, Void))
          | TRef (TFun (a,Ret rt)) -> TRef (TFun (List.map aux a, Ret (aux rt)))
          | TTempl (true, id) ->
              begin match List.assoc_opt id ts with
                | None   -> raise (Invalid_argument "")
                | Some t -> t
              end
          | TTempl (false, id) ->
              begin match List.assoc_opt id ts with
                | Some (TRef t) -> TNullRef t
                | _             -> raise (Invalid_argument "")
              end
          | _ -> Stdlib.failwith "bad types in template resolution"
        end
      in

      List.map aux
    in
    
    let matches = aux [] ts in
    try
      let resolution = resolve_solution matches (List.map snd ts) in
      if List.for_all2 subtype (List.map fst ts) resolution then
        matches
      else
        []
    with Invalid_argument _ -> []


  exception TemplateError of string node

  type t = (string * ty) list

  let rec resolve_ty (ts:t) (t : ty node) : ty node =
    begin match t.t with
      | TInt | TFlt | TChar | TBool | TRef TStr | TRef (TNamed _) | TRef (TModNamed _) -> t
      | TNullRef rty ->
          let t' = resolve_ty ts (ofnode (TRef rty) t) in
          begin match t'.t with
            | TRef rty -> ofnode (TNullRef rty) t
            | _        -> raise @@ TemplateError (ofnode "bad maybe-null template matching" t)
          end
      | TTempl (true,id) ->
          begin match List.assoc_opt id ts with
            | Some st -> ofnode st t
            | None    -> raise @@ TemplateError (ofnode (Printf.sprintf "type %s not resolved" id) t)
          end
      | TTempl (false,id) ->
          begin match List.assoc_opt id ts with
            | Some (TRef rt) -> ofnode (TNullRef rt) t
            | Some _         -> raise @@ TemplateError (ofnode (Printf.sprintf "maybe-null generic type requires non-null reference type resolution") t)
            | None           -> raise @@ TemplateError (ofnode (Printf.sprintf "type %s not resolved" id) t)
          end
      | TRef (TArr et) -> ofnode (TRef (TArr (resolve_ty ts (ofnode et t)).t)) t
      | TRef (TFun (at,retty)) ->
          let at' = List.map (fun at -> (resolve_ty ts (ofnode at t)).t) at in
          let retty' =
            begin match retty with
              | Void   -> Void
              | Ret rt -> Ret (resolve_ty ts (ofnode rt t)).t
            end in
          ofnode (TRef (TFun (at',retty'))) t
    end

  let rec resolve_exp (ts:t) (e : exp node) : exp node =
    let r_t, r_e = resolve_ty ts, resolve_exp ts in
    ofnode
      begin match e.t with
        | EmptyList (Some t) -> EmptyList (Some (r_t t))
        | Null (Some rt)     -> Null (Some (r_t rt))
        
        | LitArr es               -> LitArr (List.map r_e es)
        | Deref  exp              -> Deref (r_e exp)
        | RangeList (e1,i1,i2,e2) -> RangeList (r_e e1, i1, i2, r_e e2)
        | ListComp (e1,es,e2)     -> ListComp (r_e e1, List.map (fun (id,e) -> id, r_e e) es, r_e e2)
        | Ternary (e1,e2,e3)      -> Ternary (r_e e1, r_e e2, r_e e3)
        | Sprintf (fmt,s,es)      -> Sprintf (fmt, s, List.map r_e es)
        | Bop (op,e1,e2)          -> Bop (op, r_e e1, r_e e2)
        | Uop (op,e1)             -> Uop (op, r_e e1)
        | Cmps (e1,es)            -> Cmps (r_e e1, List.map (fun (t,e) -> t, r_e e) es)
        | FApp (f,es)             -> FApp (r_e f, List.map (fun e' -> ofnode (match e'.t with | None -> None | Some e'' -> Some (r_e (ofnode e'' e')).t) e) es)
        | Subscript (e1,e2)       -> Subscript (r_e e1, r_e e2)
        | Proj (e1,s)             -> Proj (r_e e1, s)

        | _ -> e.t
      end
      e

  let rec resolve_stmt (ts:t) (s : stmt node) : stmt node =
    let r_t, r_e, r_s, r_b = resolve_ty ts, resolve_exp ts, resolve_stmt ts, resolve_block ts in
    ofnode
    begin match s.t with
      | VDecl (id,m,None,e)    -> VDecl (id,m,None,r_e e)
      | VDecl (id,m,Some t,e)  -> VDecl (id,m,Some (r_t t),r_e e)
      | Assert e               -> Assert (r_e e)
      | Assn (e1,e2)           -> Assn (r_e e1, r_e e2)
      | Expr e                 -> Expr (r_e e)
      | If (e,b1,b2)           -> If (r_e e, r_b b1, r_b b2)
      | Denull (id,e,b1,b2)    -> Denull (id, r_e e, r_b b1, r_b b2)
      | While (e,b)            -> While (r_e e, r_b b)
      | DoWhile (e,b)          -> DoWhile (r_e e, r_b b)
      | For (id,e1,i1,i2,e2,b) -> For (id, r_e e1, i1, i2, r_e e2, r_b b)
      | ForIn (id,e,b)         -> ForIn (id, r_e e, r_b b)
      | Break                  -> Break
      | Continue               -> Continue
      | Return None            -> Return None
      | Return (Some e)        -> Return (Some (r_e e))
    end
    s
  and resolve_block (ts:t) : stmt node list -> stmt node list = List.map (resolve_stmt ts)

  let resolve_f (ts:t) (args : ty node list) (rt : retty node) (b : stmt node list) : ty node list * retty node * stmt node list =
    List.map (resolve_ty ts) args,
    begin match rt.t with
      | Void  -> ofnode Void rt
      | Ret t -> ofnode (Ret (resolve_ty ts (ofnode t rt)).t) rt
    end,
    resolve_block ts b

end