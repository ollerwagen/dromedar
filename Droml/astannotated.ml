open Ast

type exp' =
  | Id        of string
  | ModAccess of string * string
  | LitInt    of Token.tint
  | LitFlt    of Token.tflt
  | LitChar   of Token.tchar
  | LitBool   of Token.tbool
  | LitStr    of string
  | LitArr    of annt_exp list
  | Deref     of annt_exp
  | EmptyList of ty
  | RangeList of annt_exp * inclusion * inclusion * annt_exp
  | ListComp  of annt_exp * (string * annt_exp) list * annt_exp
  | Ternary   of annt_exp * annt_exp * annt_exp
  | Null      of rty
  | Sprintf   of formatstr * string * annt_exp list
  | Bop       of bop * annt_exp * annt_exp
  | Uop       of uop * annt_exp
  | Cmps      of annt_exp * (cmpop * annt_exp) list
  | FApp      of annt_exp * annt_exp list
  | Subscript of annt_exp * annt_exp
  | Proj      of annt_exp * string
and annt_exp = exp' * ty

type annt_stmt =
  | VDecl   of string * mutability * ty option * annt_exp
  | Assert  of annt_exp
  | Assn    of annt_exp * annt_exp
  | Expr    of exp' * ty option
  | If      of annt_exp * annt_stmt list * annt_stmt list
  | Denull  of string * annt_exp * annt_stmt list * annt_stmt list
  | While   of annt_exp * annt_stmt list
  | DoWhile of annt_exp * annt_stmt list
  | For     of string * annt_exp * inclusion * inclusion * annt_exp * annt_stmt list
  | Return  of annt_exp option

type annt_gstmt =
  | GVDecl  of string * mutability * ty option * annt_exp
  | GFDecl  of string * (string * ty) list * retty * annt_stmt list
  | Module  of string
  | GNFDecl of string * (string * ty) list * retty
  | GNVDecl of string * ty
  | GNTDecl of string

type annt_program = annt_gstmt list

let rec print_ty (t : ty) : string =
  begin match t with
    | TInt       -> "int"
    | TFlt       -> "flt"
    | TChar      -> "char"
    | TBool      -> "bool"
    | TRef r     -> print_rty r
    | TNullRef r -> Printf.sprintf "%s?" (print_rty r)
  end
and print_retty (t : retty) : string =
  begin match t with
    | Void  -> "void"
    | Ret r -> print_ty r
  end
and print_rty (r:rty) : string =
  begin match r with
    | TStr   -> "string"
    | TArr t -> Printf.sprintf "[%s]" (print_ty t)
    | TFun (args,rt) ->
        Printf.sprintf "(%s) -> %s" (String.concat ", " (List.map (fun arg -> print_ty arg) args)) (print_retty rt)
    | TNamed id        -> id
    | TModNamed (m,id) -> Printf.sprintf "%s.%s" m id
  end

let print_incl (i : inclusion * inclusion) : string =
  begin match i with 
    | Incl,Incl -> "..."
    | Incl,Excl -> "..|"
    | Excl,Incl -> "|.."
    | Excl,Excl -> "|.|"
  end

let rec print_exp (e:annt_exp) : string =
  begin match fst e with
    | Id        s           -> s
    | ModAccess (m,id)      -> Printf.sprintf "%s.%s"  m id
    | LitInt    i           -> Printf.sprintf "%Ld"    i
    | LitFlt    f           -> Printf.sprintf "%f"     f
    | LitChar   c           -> Printf.sprintf "'%c'"   c
    | LitBool   b           -> Printf.sprintf "%B"     b
    | LitStr    s           -> Printf.sprintf "\"%s\"" (String.escaped s)
    | LitArr    ls          -> Printf.sprintf "[%s]" (String.concat ", " (List.map print_exp ls))
    | Deref     e           -> Printf.sprintf "assert %s" (print_exp e)
    | EmptyList t           -> Printf.sprintf "([] of %s)" (print_ty t)
    | RangeList (s,i1,i2,e) -> Printf.sprintf "[%s%s%s]" (print_exp s) (print_incl (i1,i2)) (print_exp e)
    | ListComp  (e,vs,c)    -> Printf.sprintf "[%s : %s : %s]" (print_exp e) (String.concat ", " (List.map (fun (id,e) -> Printf.sprintf "%s in %s" id (print_exp e)) vs)) (print_exp c)
    | Ternary   (c,e1,e2)   -> Printf.sprintf "(?%s -> %s : %s)" (print_exp c) (print_exp e1) (print_exp e2)
    | Null      t           -> Printf.sprintf "(null of %s)" (print_rty t)
    | Sprintf   (t,s,es)    -> Printf.sprintf "%sprintf(\"%s\"%s)" (if t = Sprintf then "s" else "") (String.escaped s) (String.concat "" (List.map (fun e -> ", " ^ print_exp e) es))
    | Bop       (o,l,r)     -> Printf.sprintf "(%s %s %s)" (print_exp l) (List.assoc o bop_string) (print_exp r)
    | Uop       (o,e)       -> Printf.sprintf "(%s%s)" (List.assoc o uop_string) (print_exp e)
    | Cmps      (e,ls)      -> Printf.sprintf "(%s%s)" (print_exp e) (List.fold_left (fun s (o,e) -> Printf.sprintf "%s %s %s" s (List.assoc o cmp_string) (print_exp e)) "" ls)
    | FApp      (f,ls)      -> Printf.sprintf "%s(%s)" (print_exp f) (String.concat ", " (List.map print_exp ls))
    | Subscript (l,r)       -> Printf.sprintf "%s[%s]" (print_exp l) (print_exp r)
    | Proj      (e,s)       -> Printf.sprintf "%s.%s" (print_exp e) s
  end

let rec print_stmt (indent:int) (s:annt_stmt) : string =
  let ind = String.make (2*indent) ' ' in
  begin match s with
    | VDecl   (id,Const,None,e)   -> Printf.sprintf "%slet %s := %s\n"              ind id (print_exp e)
    | VDecl   (id,Const,Some t,e) -> Printf.sprintf "%slet %s:%s := %s\n"           ind id (print_ty t) (print_exp e)
    | VDecl   (id,Mut,None,e)     -> Printf.sprintf "%smut %s := %s\n"              ind id (print_exp e)
    | VDecl   (id,Mut,Some t,e)   -> Printf.sprintf "%smut %s:%s := %s\n"           ind id (print_ty t) (print_exp e)
    | Assert  e                   -> Printf.sprintf "%sassert %s\n"                 ind (print_exp e)
    | Assn    (l,r)               -> Printf.sprintf "%s%s := %s\n"                  ind (print_exp l) (print_exp r)
    | Expr    (e,t)               -> Printf.sprintf "%s%s\n"                        ind (print_exp (e,TInt))
    | If      (c,t,n)             -> Printf.sprintf "%sif %s\n%s%selse\n%s"         ind (print_exp c) (print_block (indent+1) t) ind (print_block (indent+1) n)
    | Denull  (id,e,t,n)          -> Printf.sprintf "%sdenull %s := %s\n%s%selse\n%s" ind id (print_exp e) (print_block (indent+1) t) ind (print_block (indent+1) n)
    | While   (c,b)               -> Printf.sprintf "%swhile %s\n%s"                ind (print_exp c) (print_block (indent+1) b)
    | DoWhile (c,b)               -> Printf.sprintf "%sdo\n%s\n%swhile %s\n"        ind (print_block (indent+1) b) ind (print_exp c)
    | For     (i,s,i1,i2,e,b)     -> Printf.sprintf "%sfor %s := %s %s %s\n%s"      ind i (print_exp s) (print_incl (i1,i2)) (print_exp e) (print_block (indent+1) b)
    | Return  None                -> Printf.sprintf "%sreturn\n"                    ind
    | Return  (Some e)            -> Printf.sprintf "%sreturn %s\n"                 ind (print_exp e)
  end
and print_block (indent:int) (b : annt_stmt list) : string =
  (String.concat "" (List.map (print_stmt (indent+1)) b))

let print_gstmt (gs:annt_gstmt) : string =
  begin match gs with
    | GVDecl (id,Const,None,e)   -> Printf.sprintf "global %s := %s\n\n" id (print_exp e)
    | GVDecl (id,Const,Some t,e) -> Printf.sprintf "global %s:%s := %s\n\n" id (print_ty t) (print_exp e)
    | GVDecl (id,Mut,None,e)     -> Printf.sprintf "global mut %s := %s\n\n" id (print_exp e)
    | GVDecl (id,Mut,Some t,e)   -> Printf.sprintf "global mut %s:%s := %s\n\n" id (print_ty t) (print_exp e)
    | GFDecl (id,[],rt,b)        -> Printf.sprintf "fn %s -> %s\n%s\n" id (print_retty rt) (print_block 1 b)
    | GFDecl (id,args,rt,b)      -> Printf.sprintf "fn %s : %s -> %s\n%s\n" id (String.concat ", " (List.map (fun (s,t) -> Printf.sprintf "%s:%s" s (print_ty t)) args)) (print_retty rt) (print_block 1 b)
    | Module m                   -> Printf.sprintf "module %s\n\n\n" m
    | GNFDecl (id,ats,rt)        -> Printf.sprintf "native fn %s%s %s -> %s\n" id (if ats=[] then "" else ":") (String.concat ", " (List.map (fun (id,t) -> id ^ ":" ^ print_ty t) ats)) (print_retty rt)
    | GNVDecl (id,t)             -> Printf.sprintf "native %s: %s\n" id (print_ty t)
    | GNTDecl id                 -> Printf.sprintf "native type %s\n" id
  end

let print_program (prog:annt_program) : string =
  String.concat "" @@ List.map print_gstmt prog