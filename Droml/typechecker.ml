open Common
open Token
open Ast
open Astannotated
open Templateresolver
open Typerelations

module TypeChecker = struct

  exception TypeError of string node

  let gensym : string -> string =
    let n = ref 0 in
    (fun id -> incr n; Printf.sprintf "%s%d_" id !n)

  module Ctxt = struct

    (* List.hd @@ List.assoc (fst l) (snd l)    <=>    top level block in current module *)
    (* per module: string list is list of recognized type names *)
    type t =
      { current    : string
      ; bindings   : (string * ((string * (ty * mutability)) list list * string list)) list
      ; templfs    : ((string * string) * ((string * ty node) list * retty node * stmt node list)) list
      ; res_templs : (((string * string) * (string * ty) list) * (string * string * bool)) list
      }

    (* invariant: every file starts with a module declaration (ensured by the main.ml file) *)
    let empty : t = { current = "" ; bindings = [] ; templfs = [] ; res_templs = [] }

    let get_current_module (c:t) : string = c.current

    let set_current_module (c:t) (id:string) : t =
      if List.mem_assoc id c.bindings then
        { c with current = id }
      else
        { c with current = id ; bindings = (id, ([[]], [])) :: c.bindings }

    let exists_module (c:t) (m:string) : bool = List.mem_assoc m c.bindings
    
    let has_in_module (c:t) (m:string) (id:string) : bool =
      if List.mem_assoc m c.bindings then
        List.exists (List.mem_assoc id) (fst (List.assoc m c.bindings))
      else
        false

    let has (c:t) (id:string) : bool =
      has_in_module c c.current id

    let has_toplevel (c:t) (id:string) : bool =
      List.mem_assoc id (List.hd (fst (List.assoc c.current c.bindings)))

    let has_in_any_binding (c:t) (id:string) : bool =
      List.exists (fun (_,(l,_)) -> List.exists (List.mem_assoc id) l) c.bindings

    let has_modnamedt (c:t) (m:string) (id:string) : bool =
      if List.mem_assoc m c.bindings then
        List.mem id (snd (List.assoc m c.bindings))
      else
        false

    let has_namedt (c:t) (id:string) : bool =
      has_modnamedt c c.current id

    let add_modnamedt (c:t) (m:string) (id:string) : t =
      let l,c' =
        if List.mem_assoc m c.bindings then
          List.assoc m c.bindings, List.remove_assoc m c.bindings
        else
          ([[]], []), c.bindings
        in
      { c with bindings = (m, (fst l, id :: snd l)) :: c' }

    let add_namedt (c:t) (id:string) : t =
      add_modnamedt c c.current id

    let get_from_module (c:t) (m:string) (id:string) : ty * mutability =
      let l = fst (List.assoc m c.bindings) in
      let found,res =
        List.fold_left (fun (f,r) l -> if f then true,r else if List.mem_assoc id l then true, List.assoc id l else false,r) (false,(TInt,Const)) l in
      if found then res else raise Not_found

    let get (c:t) (id:string) : ty * mutability =
      get_from_module c c.current id

    let get_from_any_binding (c:t) (id:string) : ty * mutability =
      let rec aux (m : (string * ((string * (ty * mutability)) list list * string list)) list) : ty * mutability =
        begin match m with
          | []    -> raise Not_found
          | x::xs -> if List.exists (List.mem_assoc id) (fst (snd x)) then get_from_module c (fst x) id else aux xs
        end
      in
      aux c.bindings

    let add_level (c:t) : t =
      let l = List.assoc c.current c.bindings in
      let c' = List.remove_assoc c.current c.bindings in
      { c with bindings = (c.current, ([] :: fst l, snd l)) :: c' }

    let add_binding_to_module (c:t) (m:string) (bnd : string*(ty*mutability)) : t =
      let l,c' =
        if List.mem_assoc m c.bindings then
          List.assoc m c.bindings, List.remove_assoc m c.bindings
        else
          ([[]], []), c.bindings
        in
      { c with bindings = (m, ((bnd :: List.hd (fst l)) :: List.tl (fst l), snd l)) :: c' }

    let add_binding (c:t) (bnd : string * (ty * mutability)) : t =
      add_binding_to_module c c.current bnd
    
    let add_generic_function_to_module (c:t) (m:string) (id:string) (bnd : (string * ty node) list * retty node * stmt node list) : t =
      { c with templfs = ((m,id),bnd) :: c.templfs }

    let add_generic_function (c:t) (id:string) (bnd : (string * ty node) list * retty node * stmt node list) : t =
      add_generic_function_to_module c (get_current_module c) id bnd

    let has_generic_function_in_module (c:t) (m:string) (id:string) : bool =
      List.mem_assoc (m,id) c.templfs
    
    let has_generic_function (c:t) (id:string) : bool =
      has_generic_function_in_module c (get_current_module c) id

    let get_generic_function_tys (c:t) (m:string) (id:string) : (string * ty node) list * retty node =
      let a,r,_ = List.assoc (m,id) c.templfs in a,r

    let get_generic_function (c:t) (m:string) (id:string) : (string * ty node) list * retty node * stmt node list =
      List.assoc (m,id) c.templfs
    
    let resolve_generic_function_in_module (c:t) (m:string) (id:string) (ts : (string * ty) list) : t * (string * string) =
      begin match List.assoc_opt ((m,id),ts) c.res_templs with
        | None ->
            let id' = gensym id in
            { c with res_templs = (((m, id), ts), (m, id', true)) :: c.res_templs }, (m, id')
        | Some (m,id,_) -> c, (m, id)
      end
    
    let resolve_generic_function (c:t) (id:string) (ts : (string * ty) list) : t * (string * string) =
      resolve_generic_function_in_module c (get_current_module c) id ts

    let copy_resolved_generics_from (c_from:t) (c_to:t) : t = { c_to with res_templs = c_from.res_templs }

    (* first is the resolved functions' name, then its type mappings, then the unresolved function's name *)
    let get_resolved_generics_todo_list (c:t) : t * ((string * string) * (string * ty) list * (string * string)) list =
      let templfs',res = List.fold_left
        (fun (templfs,res) ((genname,resolution),(res_m,res_id,todo)) ->
          if todo then
            ((genname,resolution),(res_m,res_id,false)) :: templfs, ((res_m,res_id), resolution, genname) :: res
          else
            ((genname,resolution),(res_m,res_id,todo)) :: templfs, res
        )
        ([],[]) c.res_templs in
      { c with res_templs = templfs' }, res

    let has_resolved_generics_todo (c:t) : bool = List.exists (fun (_,(_,_,b)) -> b) c.res_templs

  end
  
  let startcontext : Ctxt.t = Ctxt.empty

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
    ; Div, numop ; Mod, intop
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
      | TInt | TFlt | TChar | TBool | TRef TStr | TNullRef TStr-> true
      | TRef (TArr t) | TNullRef (TArr t) -> is_printable t
      | _ -> false
    end

  let rec check_ty (c:Ctxt.t) (allow_templ : bool) (t : ty node) : unit =
    begin match t.t with
      | TRef (TNamed id)        | TNullRef (TNamed id)        ->
          if Ctxt.has_namedt c id then ()
          else raise @@ TypeError (ofnode (Printf.sprintf "Type %s doesn't exist" id) t)
      | TRef (TModNamed (m,id)) | TNullRef (TModNamed (m,id)) ->
          if Ctxt.has_modnamedt c m id then ()
          else raise @@ TypeError (ofnode (Printf.sprintf "Type %s.%s doesn't exist" m id) t)
      | TRef (TArr st) | TNullRef (TArr st) -> check_ty c allow_templ (ofnode st t)
      | TTempl _ -> if allow_templ then () else raise @@ TypeError (ofnode (Printf.sprintf "Template types not allowed here") t)
      | _ -> ()
    end

  let void_placeholder : ty = TRef (TModNamed ("",""))
  let templ_placeholder : ty = TTempl (true,"")
  
  let rec check_exp (c:Ctxt.t) (exp_t : ty option) (perm_void : bool) (e : exp node) : Ctxt.t * annt_exp =
    begin match e.t with
      | Id id ->
          if Ctxt.has c id then c, (Id id, fst (Ctxt.get c id))
          else raise @@ TypeError (ofnode (Printf.sprintf "Variable '%s' not declared" id) e)
      | LitInt  i   -> c, (LitInt i, TInt)
      | LitFlt  f   -> c, (LitFlt f, TFlt)
      | LitChar cc  -> c, (LitChar cc, TChar)
      | LitBool b   -> c, (LitBool b, TBool)
      | LitStr  s   -> c, (LitStr s, TRef TStr)
      | LitArr  ls  ->
          let exp_t' =
            begin match exp_t with
              | Some (TRef (TArr t)) | Some (TNullRef (TArr t)) -> Some t
              | _                                               -> None
            end in
          let c', annt_es = List.fold_left (fun (c,res) e -> let c',ae = check_exp c exp_t' false e in c', res @ [ae]) (c,[]) ls in
          let spts = List.map suptys @@ List.map snd annt_es in
          begin match intersect spts with
            | []    -> raise @@ TypeError (ofnode "Types in array must have a common supertype" e)
            | t::ts -> c', (LitArr annt_es, TRef (TArr (List.fold_left (fun m t -> if subtype t m then t else m) t ts)))
          end
      | Deref e ->
          let c',e' = check_exp c None false e in
          begin match snd e' with
            | TNullRef t -> c', (Deref e', TRef t)
            | _          -> raise @@ TypeError (ofnode "Expression assertion must take maybe-null type argument" e)
          end
      | EmptyList None ->
          begin match exp_t with
            | Some (TRef (TArr t)) | Some (TNullRef (TArr t)) -> c, (EmptyList t, TRef (TArr t))
            | Some _               -> raise @@ TypeError (ofnode "List type doesn't match here" e)
            | _                    -> raise @@ TypeError (ofnode "Cannot infer empty list element type" e)
          end
      | EmptyList (Some t) -> let () = check_ty c false t in c, (EmptyList t.t, TRef (TArr t.t))
      | RangeList (e1,i1,i2,e2) ->
          let c', e1' = check_exp c  (Some TInt) false e1 in
          let c'',e2' = check_exp c' (Some TInt) false e2 in
          begin match snd e1', snd e2' with
            | TInt,TInt   -> c'', (RangeList (e1',i1,i2,e2'), TRef (TArr TInt))
            | TChar,TChar -> c'', (RangeList (e1',i1,i2,e2'), TRef (TArr TChar))
            | _           -> raise @@ TypeError (ofnode "Range list expressions should be of type int" e)
          end
      | ListComp (ex,vs,cnd) ->
          let exp_t' =
            begin match exp_t with
              | Some (TRef (TArr t)) | Some (TNullRef (TArr t)) -> Some t
              | _                                               -> None
            end in
          let c',avs =
            List.fold_left
              (fun (c,res) (id,exp) ->
                let c',(exp',t) = check_exp c None false exp in
                begin match t with
                  | TRef (TArr t') -> Ctxt.add_binding c' (id,(t',Const)), res @ [ id, (exp',t) ]
                  | _              -> raise @@ TypeError (ofnode "list comprehension variable should be in an array type" e)
                end
              )
              (c,[]) vs in
          let c'', ae = check_exp c' exp_t' false ex in
          let c''', acnd = check_exp c'' (Some TBool) false cnd in
          Ctxt.copy_resolved_generics_from c''' c, (ListComp (ae,avs,acnd), TRef (TArr (snd ae)))
      | Ternary (cnd,e1,e2) ->
          let c',cnd' = check_exp c (Some TBool) false cnd in
          let c'',e1' = check_exp c' None false e1 in
          let c''',e2' = check_exp c'' None false e2 in
          if snd cnd' = TBool then
            begin match intersect_single (suptys (snd e1')) (suptys (snd e2')) with
              | []    -> raise @@ TypeError (ofnode "types in ternary expression must have common supertype" e)
              | t::ts -> c''', (Ternary (cnd',e1',e2'), List.fold_left (fun m t -> if subtype t m then t else m) t ts)
            end
          else
            raise @@ TypeError (ofnode "ternary condition must be of type bool" cnd)
      | Null None -> 
          begin match exp_t with
            | Some (TNullRef rt) -> c, (Null rt, TNullRef rt)
            | Some _             -> raise @@ TypeError (ofnode "Maybe-null reference type doesn't match here" e)
            | _                  -> raise @@ TypeError (ofnode "cannot infer null type" e)
          end
      | Null (Some t) -> 
          begin match t.t with
            | TRef rt -> 
                let () = check_ty c false (ofnode (TNullRef rt) t) in c, (Null rt, TNullRef rt)
            | _ ->
                raise @@ TypeError (ofnode "in null of t: type must be non-null reference type" t)
          end
      | Sprintf (pft, s, es) ->
          let () =
            if pft = Printf && not perm_void then
              raise @@ TypeError (ofnode "cannot use printf in non-void expression" e)
            else () in
          let c',annt_es = List.fold_left (fun (c,res) e -> let c',e' = check_exp c None false e in c', res @ [e']) (c,[]) es in
          let indexstrs = Str.full_split (Str.regexp "{\\d+}") s.t in
          let strindices = List.filter_map (function | Str.Delim s -> Some (Stdlib.int_of_string (String.sub s 1 (String.length s - 2))) | _ -> None) indexstrs in
          if List.for_all (fun i -> 0 <= i && i < List.length annt_es) strindices && List.for_all (fun (_,t) -> is_printable t) annt_es then
            c', (Sprintf (pft, s.t, annt_es), if pft = Printf then void_placeholder else  TRef TStr)
          else
            raise @@ TypeError (ofnode "Something is wrong with this sprintf expression" e)
      | Uop (op,r) ->
          let opts = List.assoc op uop_types in
          let c',expt = check_exp c None false r in
          begin match List.assoc_opt (snd expt) opts with
            | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand type %s" (List.assoc op uop_string) (Ast.print_ty (ofnode (snd expt) r))) r)
            | Some t -> c', (Uop (op, expt), t)
          end
      | Bop (op,l,r) ->
          let opts = List.assoc op bop_types in
          let c',lt = check_exp c None false l in
          let c'',rt = check_exp c None false r in
          begin match snd lt, snd rt, op with
            | TRef (TArr t1), TRef (TArr t2), Add ->
                begin match intersect_single (suptys t1) (suptys t2) with
                  | []    -> raise @@ TypeError (ofnode "Types in array must have a common supertype" e)
                  | t::ts -> c'', (Bop (op,lt,rt), TRef (TArr (List.fold_left (fun m t -> if subtype t m then t else m) t ts)))
                end
            | _ ->
                begin match List.assoc_opt (snd lt, snd rt) opts with
                  | None   -> raise @@ TypeError (ofnode (Printf.sprintf "Operation %s undefined for operand types (%s,%s)" (List.assoc op bop_string) (Ast.print_ty (ofnode (snd lt) l)) (Ast.print_ty (ofnode (snd rt) l))) e)
                  | Some t -> c'', (Bop (op, lt, rt), t)
                end
          end
      | Cmps (x,xs) ->
          let c',first_exp = check_exp c None false x in
          let _,clist,c'' =
            List.fold_left
              (fun (lt,l,c) (op,r) -> 
                let c',rt = check_exp c None false r in
                begin match op with
                  | RefEq | RefNotEq ->
                      begin match lt, snd rt with
                        | TRef _, _ | _, TRef _ | TNullRef _, _ | _, TNullRef _ ->
                            if subtype lt (snd rt) || subtype (snd rt) lt then
                              snd rt, l @ [op, rt], c'
                            else
                              raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s only takes related reference types" (List.assoc op cmp_string)) r)
                        | _ -> raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s only takes reference types" (List.assoc op cmp_string)) r)
                      end
                  | _ ->
                      if List.mem (lt, snd rt) cmpop_types then
                        snd rt, l @ [op, rt], c'
                      else
                        raise @@ TypeError (ofnode (Printf.sprintf "Comparator %s undefined for operand types (%s,%s)" (List.assoc op cmp_string) (Ast.print_ty (ofnode lt e)) (Ast.print_ty (ofnode (snd rt) r))) r)
                end
              )
              (snd first_exp, [], c') xs in
          c'', (Cmps (first_exp, clist), TBool)
      | FApp (f,args) ->
          let c',es = List.fold_left (fun (c,res) e -> match e.t with | None -> c,res@[None] | Some exp -> let c',e' = check_exp c None false (ofnode exp e) in c',res@[Some e']) (c,[]) args in
          let isgeneric,m,id =
            begin match f.t with
              | Id id ->
                  if Ctxt.has_generic_function c' id then
                    true, Ctxt.get_current_module c', id
                  else
                    false, "", ""
              | Proj (l,r) ->
                  begin match l.t with
                    | Id m ->
                        if Ctxt.has_generic_function_in_module c' m r.t then
                          true, m, r.t
                        else
                          false, "", ""
                    | _ -> false, "", ""
                  end
              | _ -> false, "", ""
            end in

          let c'',f',a,rt =
            if isgeneric then
              let argtys, retty = Ctxt.get_generic_function_tys c' m id in
              let () = if List.length argtys <> List.length args then raise @@ TypeError (ofnode "argument list length mismatch" e) else () in
              let tmatches = TemplateResolver.resolve_templates (List.combine (List.map (function | None -> None | Some(_,t) -> Some t) es) (List.map (fun (_,t) -> t.t) argtys)) in
              begin match tmatches with
                | []  -> raise @@ TypeError (ofnode "unsuccessful template match" e)
                | tms ->
                    let resolved_etys, resolved_retty = TemplateResolver.resolve_args tms (List.map snd argtys) retty in
                    let c'',(m',id') = Ctxt.resolve_generic_function_in_module c' m id tms in
                    c'', ModAccess (m', id'), List.map (fun x -> x.t) resolved_etys, resolved_retty.t
              end
            else
              begin match check_exp c' None false f with
                | c'', (f', TRef (TFun (a,rt))) ->
                    let _ = if List.length a <> List.length args then raise @@ TypeError (ofnode (Printf.sprintf "Argument list must match the length of the function argument list (type %s)" (Ast.print_ty (ofnode (TRef (TFun (a,rt))) f))) e) else () in
                    c'', f', a, rt
                | _, t -> raise @@ TypeError (ofnode (Printf.sprintf "Type %s cannot act as a function" (Ast.print_ty (ofnode (snd t) f))) f)
              end
            in

            let tlist =
              List.fold_left2
                (fun tlist (e,enode) t ->
                  match e with
                    | None    -> tlist @ [t]
                    | Some (e',t') ->
                        if subtype t' t || crosstype t' t then tlist
                        else raise @@ TypeError (ofnode (Printf.sprintf "argument type mismatch in function application") enode)
                )
                [] (List.combine es args) a in
            
            begin match tlist with
              | [] ->
                  begin match rt, perm_void with
                    | Void, true  -> c'', (FApp ((f', TRef (TFun (a,rt))), List.filter_map identity es), void_placeholder)
                    | Void, false -> raise @@ TypeError (ofnode "Function in expression must not be of void type" f)
                    | Ret t, _    -> c'', (FApp ((f', TRef (TFun (a,rt))), List.filter_map identity es), t)
                  end
              | l -> c'', (ParFApp ((f', TRef (TFun (a,rt))), es) , TRef (TFun (tlist, rt)))
            end
            
      | Subscript (l,r) ->
          let c',l1 = check_exp c None false l in
          let c'',l2 = check_exp c' (Some TInt) false r in
          begin match l1, l2 with
            | (l', TRef (TArr t)), (r', TInt) -> c'', (Subscript ((l', TRef (TArr t)), (r', TInt)), t)
            | (_, t),              (_, TInt)  -> raise @@ TypeError (ofnode (Printf.sprintf "Left-hand-side of [] expression is of type %s but should be of array type" (Ast.print_ty (ofnode t l))) l)
            | _,                   (_, t)     -> raise @@ TypeError (ofnode (Printf.sprintf "Right-hand-side of [] expression is of type %s but should be of int type" (Ast.print_ty (ofnode t r))) r)
          end
      | Proj (lhs,id) ->
          let mayberes =
            begin match lhs.t with
              | Id m ->
                  if Ctxt.exists_module c m then
                    if Ctxt.has_in_module c m id.t then
                      let t,_ = Ctxt.get_from_module c m id.t in
                      Some (ModAccess (m, id.t), t)
                    else None
                  else None
              | _ -> None
            end in
          begin match mayberes with
            | Some res -> c, res
            | _ ->
                begin match check_exp c None false lhs, id.t with
                  | (c', (e', TRef (TArr t))), "length" -> c', (Proj ((e', TRef (TArr t)), "length"), TInt)
                  | (_, (_,t)), id -> raise @@ TypeError (ofnode (Printf.sprintf "Cannot use projection with type %s and projector name %s" (Ast.print_ty (ofnode t e)) id) e)
                end
          end
    end
  
  let rec check_stmt (rt : retty) (inloop : bool) (c : Ctxt.t) (s : stmt node) : annt_stmt * Ctxt.t * bool * bool = 
    begin match s.t with
      | VDecl (id,m,t,e) ->
          if Ctxt.has_toplevel c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in this block" id) s)
          else
            begin match t with
              | None   ->
                  let c',et = check_exp c None false e in
                  VDecl (id,m,None,et), Ctxt.add_binding c' (id,(snd et,m)), false, false
              | Some t ->
                  let c',et = check_exp c (Some t.t) false e in
                  let () = check_ty c' false t in (* c and c' are equivalent here, only added for possible OCaml optimizations *)
                  if subtype (snd et) t.t || crosstype (snd et) t.t then
                    VDecl (id,m,Some t.t,et), Ctxt.add_binding c' (id,(t.t,m)), false, false
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match: %s vs %s" (Ast.print_ty t) (Ast.print_ty (ofnode (snd et) e))) s)
            end
      | Assert e ->
          let c',e' = check_exp c None false e in
          begin match snd e' with
            | TBool -> Assert (e', print_exp e), c', false, false
            | _     -> raise @@ TypeError (ofnode ("assertion expression needs to be of type bool") e)
          end
      | Assn (l,r) ->
          let c',lt = check_exp c None false l in
          let c'',rt = check_exp c' (Some (snd lt)) false r in
          if is_assignable c'' l then
            if subtype (snd rt) (snd lt) || crosstype (snd lt) (snd rt) then
              Assn (lt,rt), c'', false, false
            else
              raise @@ TypeError (ofnode (Printf.sprintf "expression type doesn't match assignment target") r)
          else
            begin match fst lt with
              | Id id -> raise @@ TypeError (ofnode (Printf.sprintf "Variable %s is immutable" id) l)
              | ModAccess (m,id) -> raise @@ TypeError (ofnode (Printf.sprintf "Variable %s.%s is immutable" m id) l)
              | _ -> raise @@ TypeError (ofnode "invalid assignment target" l)
            end
      | Expr e ->
          begin match e.t with
            | FApp _ | Sprintf (Printf,_,_) ->
                let c',(e',t) = check_exp c None true e in
                Expr (e', if t = void_placeholder then None else Some t), c', false, false
            | _ -> raise @@ TypeError (ofnode "expression statements must be function calls" e)
          end
      | If (cd,t,n) ->
          let c',ct = check_exp c None false cd in
          let c'' = Ctxt.add_level c' in
          if snd ct = TBool then
            let t',c''',r1,b1 = check_stmt_block rt inloop c'' t in
            let n',c'''',r2,b2 = check_stmt_block rt inloop (Ctxt.copy_resolved_generics_from c''' c'') n in
            If (ct,t',n'), Ctxt.copy_resolved_generics_from c'''' c', r1 && r2, b1 && b2
          else
            raise @@ TypeError (ofnode "if condition must be of type bool" cd)
      | Denull (id,e,t,n) ->
          let c',(e',et) = check_exp c None false e in
          begin match et with
            | TNullRef r ->
                let c'' = Ctxt.add_level @@ Ctxt.add_binding (Ctxt.add_level c') (id, (TRef r, Const)) in
                let t',c''',r1,b1 = check_stmt_block rt inloop c'' t in
                let n',c'''',r2,b2 = check_stmt_block rt inloop (Ctxt.copy_resolved_generics_from c''' c'') n in
                Denull (id,(e',et),t',n'), Ctxt.copy_resolved_generics_from c'''' c', r1 && r2, b1 && b2
            | _ -> raise @@ TypeError (ofnode "expression in checked cast must be a maybe-null reference" e)
          end
      | While (cd,b) ->
          let c',ct = check_exp c None false cd in
          let c'' = Ctxt.add_level c' in
          if snd ct = TBool then
            let b',c''',_,_ = check_stmt_block rt true c'' b in
            While (ct,b'), Ctxt.copy_resolved_generics_from c''' c', false, false
          else
            raise @@ TypeError (ofnode "while condition must be of type bool" cd)
      | DoWhile (cd,b) ->
          let c',ct = check_exp c None false cd in
          let c'' = Ctxt.add_level c' in
          if snd ct = TBool then
            let b',c''',r,b = check_stmt_block rt true c'' b in
            DoWhile (ct,b'), Ctxt.copy_resolved_generics_from c''' c', r, false
          else
            raise @@ TypeError (ofnode "do-while condition must be of type bool" cd)
      | For (id,exps,incl1,incl2,expe,b) ->
          let c',sty = check_exp c None false exps in
          let c'',ety = check_exp c None false expe in
          if snd sty = TInt && snd ety = TInt then
            let c''' = Ctxt.add_binding (Ctxt.add_level c'') (id,(TInt,Const)) in
            let b',c'''',_,_ = check_stmt_block rt true c''' b in
            For (id,sty,incl1,incl2,ety,b'), Ctxt.copy_resolved_generics_from c'''' c'', false, false
          else
            raise @@ TypeError (ofnode "for loops bounds must be of type int" s)
      | ForIn (id,lexp,b) ->
          let c',(l',lt) = check_exp c None false lexp in
          begin match lt with
            | TRef (TArr et) ->
                let c'' = Ctxt.add_binding (Ctxt.add_level c') (id,(et,Const)) in
                let b',c''',_,_ = check_stmt_block rt true c'' b in
                ForIn (id,(l',lt),b'), Ctxt.copy_resolved_generics_from c''' c', false, false
            | _ -> raise @@ TypeError (ofnode "for-in loop must have an array expression" lexp)
          end
      | Break ->
          if inloop then Break, c, false, true
          else raise @@ TypeError (ofnode "cannot use break statement outside of loop" s)
      | Continue ->
          if inloop then Continue, c, false, true
          else raise @@ TypeError (ofnode "cannot use continue statement outside of loop" s)
      | Return None ->
          if rt = Void then Return None, c, true, false
          else raise @@ TypeError (ofnode "cannot return without expression in non-void function" s)
      | Return (Some e) ->
          begin match rt with
            | Void  -> raise @@ TypeError (ofnode "cannot return with expression in void function" s)
            | Ret t ->
                let c',et = check_exp c (Some t) false e in
                if subtype (snd et) t || crosstype (snd et) t then
                  Return (Some et), c', true, false
                else
                  raise @@ TypeError (ofnode "return type doesn't match with function return type" s)
          end
    end
  and check_stmt_block (rt : retty) (inloop : bool) (c : Ctxt.t) : stmt node list -> (annt_stmt list * Ctxt.t * bool * bool) =
    List.fold_left (fun (b,c,r,brs) s -> if r||brs then raise (TypeError (ofnode "unreachable statement" s)) else let s',c',r',br' = check_stmt rt inloop c s in b@[s'], c', r', br') ([],c,false,false)

  let rec check_gexp (c : Ctxt.t) (ge : exp node) : annt_exp =
    begin match ge.t with
      | LitInt  i -> LitInt i,  TInt
      | LitFlt  f -> LitFlt f,  TFlt
      | LitChar c -> LitChar c, TChar
      | LitBool b -> LitBool b, TBool
      | LitStr  s -> LitStr s,  TRef TStr
      | _         -> raise @@ TypeError (ofnode "illegal global expression" ge)
    end

  let check_gstmt (c : Ctxt.t) (gs : gstmt node) : annt_gstmt option * Ctxt.t =
    begin match gs.t with
      | Module m ->
          Some (Module m), Ctxt.set_current_module c m
      | GVDecl (id,m,t,e) ->
          if Ctxt.has c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in global scope" id) gs)
          else
            let et = check_gexp c e in
            begin match t with
              | None   -> Some (GVDecl(id,m,None,et)), Ctxt.add_binding c (id,(snd et,m))
              | Some t ->
                  let () = check_ty c false t in
                  if subtype (snd et) t.t then
                    Some (GVDecl (id,m,Some t.t,et)), Ctxt.add_binding c (id,(t.t,m))
                  else
                    raise @@ TypeError (ofnode (Printf.sprintf "Declared and assigned types do not match") gs)
            end
      | GFDecl (id,args,rt,b) ->
          let () =
            if not @@ alldistinct (List.map fst args) then raise @@ TypeError (ofnode "Function argument names must be distinct" gs) else () in
          let istemplf = List.exists TemplateResolver.is_templated (List.map (fun (_,t) -> t.t) args) || (match rt.t with | Ret (TTempl _) -> true | _ -> false) in
          let () = List.iter (check_ty c istemplf) (List.map snd args) in
          let () =
            begin match rt.t with
              | Void  -> ()
              | Ret t -> check_ty c istemplf (ofnode t rt)
            end in

          if istemplf then (* templated function *)
            None, c
          else (* non-templated function *)
            let c' = Ctxt.add_level c in
            let c'' = List.fold_left (fun c (id,t) -> Ctxt.add_binding c (id,(t.t,Const))) c' args in
            let b',c''',returns,_ = check_stmt_block rt.t false c'' b in (* know that this doesn't break as break/continue only legal in loops *)
            if returns then
              Some (GFDecl (id, List.map (fun (s,t) -> s,t.t) args, rt.t, b')), Ctxt.copy_resolved_generics_from c''' c
            else if rt.t = Void then
              Some (GFDecl (id, List.map (fun (s,t) -> s,t.t) args, rt.t, b' @ [ Return None ])), Ctxt.copy_resolved_generics_from c''' c
            else
              raise @@ TypeError (ofnode "Function must return" gs)
      | GNVDecl (id,t) ->
          if Ctxt.has c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Variable %s already declared in global scope" id) gs)
          else
            Some (GNVDecl (id,t.t)), Ctxt.add_binding c (id,(t.t,Const))
      | GNFDecl (id,args,rt) ->
          let () = List.iter (check_ty c false) args in
          let () =
            begin match rt.t with
              | Void  -> ()
              | Ret t -> check_ty c false (ofnode t rt)
            end in
          Some (GNFDecl (id, List.map (fun t -> t.t) args, rt.t)), c
      | GNTDecl id ->
          if Ctxt.has_namedt c id then
            raise @@ TypeError (ofnode (Printf.sprintf "Native type %s already declared" id) gs)
          else
            Some (GNTDecl id), Ctxt.add_namedt c id
    end
  
  let check_gstmt_program (c:Ctxt.t) (gs : gstmt node list) : (annt_gstmt list * Ctxt.t) =
    List.fold_left (fun (l,c) gs -> match check_gstmt c gs with | None,c' -> l,c' | Some ag, c' -> l@[ag],c') ([],c) gs

  let create_fctxt (c : Ctxt.t) (gs : gstmt node) : Ctxt.t =
    begin match gs.t with
      | Module m              -> Ctxt.set_current_module c m
      | GFDecl (id,args,rt,b) ->
          if List.exists TemplateResolver.is_templated (List.map (fun (_,t) -> t.t) args) || (match rt.t with | Ret (TTempl _) -> true | _ -> false) then
            Ctxt.add_generic_function c id (args,rt,b)
          else
            Ctxt.add_binding c (id,(TRef (TFun (List.map (fun (_,t) -> t.t) args, rt.t)), Const))
      | GNFDecl (id,args,rt)  -> Ctxt.add_binding c (id,(TRef (TFun (List.map (fun t -> t.t) args, rt.t)), Const))
      | GVDecl _ | GNVDecl _ | GNTDecl _ -> c
    end

  let rec create_template_calls (c : Ctxt.t) (prog : annt_gstmt list) (maxdepth : int) : annt_gstmt list =
    let step (c : Ctxt.t) (res : annt_gstmt list) : Ctxt.t * annt_gstmt list =
      let c',todos = Ctxt.get_resolved_generics_todo_list c in
      List.fold_left
        (fun (c,res) ((res_m,res_id),ts,(gen_m,gen_id)) ->
          let args,retty,b = Ctxt.get_generic_function c gen_m gen_id in
          let args',retty',b' = TemplateResolver.resolve_f ts (List.map snd args) retty b in
          let fprogpart = Ast.[
              ofnode (Module res_m) retty
            ; ofnode (GFDecl (res_id, List.combine (List.map fst args) args', retty', b')) retty
            ] in
          let gs_l, c' = check_gstmt_program c fprogpart in
          c', res @ gs_l
        )
        (c',res) todos
    in
    if maxdepth = 0 then
      if Ctxt.has_resolved_generics_todo c then
        raise @@ TypeError { t = "too deep level of template resolution" ; start = 0 ; length = 0 }
      else
        prog
    else
      let c',prog' = step c prog in
      create_template_calls c' prog' (maxdepth - 1)
  
  let create_fctxt_program : Ctxt.t -> gstmt node list -> Ctxt.t = List.fold_left create_fctxt

  let check_program (prog : gstmt node list) : annt_gstmt list =
    let c = create_fctxt_program startcontext prog in
    let _ =
      if Ctxt.has_in_any_binding c "main" then
        begin match Ctxt.get_from_any_binding c "main" with
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
    let prog', c' = check_gstmt_program c prog in
    create_template_calls c' prog' 10

end