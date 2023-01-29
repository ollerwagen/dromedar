open Ast
open Common

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
    | TRef (TArr t1), TRef (TArr t2) -> t1 = t2
    | TTempl _, _ | _, TTempl _      -> Stdlib.failwith "templates not allowed in subtype relation"
    | t1,             t2             -> t1 = t2
  end

let rec subtys (t:ty) : ty list =
  begin match t with
    | TInt | TFlt | TChar | TBool -> [t]
    | TNullRef t                  ->
        let sbts = subtys (TRef t) in
        sbts @ (List.concat @@ List.map (fun t -> begin match t with | TRef t -> [TNullRef t] | _ -> [] end) sbts)
    | TRef (TArr t)               -> [ TRef (TArr t) ]
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
    | TRef (TNamed _) | TRef (TModNamed _) -> [t]
    | TTempl _ -> Stdlib.failwith "templates not allowed in subtys() call"
  end
and suptys (t:ty) : ty list =
  begin match t with
    | TInt | TFlt | TChar | TBool -> [t]
    | TNullRef t                  ->
        let spts = suptys (TRef t) in
        List.filter (function | TNullRef _ -> true | _ -> false) spts
    | TRef (TArr t)               -> [ TRef (TArr t) ; TNullRef (TArr t) ]
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
    | TRef (TNamed id)        -> [TRef (TNamed id)        ; TNullRef (TNamed id)]
    | TRef (TModNamed (m,id)) -> [TRef (TModNamed (m,id)) ; TNullRef (TModNamed (m,id))]
    | TTempl _ -> Stdlib.failwith "templates not allowed in suptys() call"
  end