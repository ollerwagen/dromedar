open Common
open Token

type uop =
  | Not
  | Neg

let uop_string : (uop * string) list =
  [ Not, "!" ; Neg, "-" ]

type bop =
  | Pow
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | Shl
  | Shr
  | Sha
  | Bitand
  | Bitxor
  | Bitor
  | Logand
  | Logxor
  | Logor

let bop_string : (bop * string) list =
  [ Pow, "**" ; Mul, "*" ; Div, "/" ; Mod, "%"
  ; Add, "+" ; Sub, "-"
  ; Shl, "<<" ; Shr, ">>" ; Sha, ">>>"
  ; Bitand, "&" ; Bitxor, "^" ; Bitor, "|"
  ; Logand, "&&" ; Logxor, "^^" ; Logor, "||" ]

type cmpop =
  | Eq
  | Neq
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | RefEq
  | RefNotEq

let cmp_string : (cmpop * string) list =
  [ Eq, "=" ; Neq, "!=" ; Less, "<" ; Greater, ">" ; LessEq, "<=" ; GreaterEq, ">=" ; RefEq, "==" ; RefNotEq, "!==" ]

type mutability =
  | Const
  | Mut

type inclusion =
  | Incl
  | Excl

type ty =
  | TInt | TFlt | TChar | TBool
  | TRef of rty
  | TNullRef of rty
and rty =
  | TStr
  | TArr of ty
  | TFun of ty list * retty
  | TNamed of string
  | TModNamed of string * string
and retty =
  | Void
  | Ret of ty

type formatstr = | Printf | Sprintf

type exp =
  | Id        of string
  | LitInt    of Token.tint
  | LitFlt    of Token.tflt
  | LitChar   of Token.tchar
  | LitBool   of Token.tbool
  | LitStr    of string
  | LitArr    of exp node list
  | Deref     of exp node
  | EmptyList of ty node option
  | RangeList of exp node * inclusion * inclusion * exp node
  | ListComp  of exp node * (string * exp node) list * exp node
  | Ternary   of exp node * exp node * exp node
  | Null      of rty node option
  | Sprintf   of formatstr * string node * exp node list
  | Bop       of bop * exp node * exp node
  | Uop       of uop * exp node
  | Cmps      of exp node * (cmpop * exp node) list
  | FApp      of exp node * exp option node list
  | Subscript of exp node * exp node
  | Proj      of exp node * string node

type stmt =
  | VDecl   of string * mutability * ty node option * exp node
  | Assert  of exp node
  | Assn    of exp node * exp node
  | Expr    of exp node 
  | If      of exp node * stmt node list * stmt node list
  | Denull  of string * exp node * stmt node list * stmt node list
  | While   of exp node * stmt node list
  | DoWhile of exp node * stmt node list
  | For     of string * exp node * inclusion * inclusion * exp node * stmt node list
  | ForIn   of string * exp node * stmt node list
  | Break
  | Continue
  | Return  of exp node option

type gstmt =
  | GVDecl  of string * mutability * ty node option * exp node
  | GFDecl  of string * (string * ty node) list * retty node * stmt node list
  | Module  of string
  | GNFDecl of string * ty node list * retty node
  | GNVDecl of string * ty node
  | GNTDecl of string

type program = gstmt node list

let rec print_ty (t : ty node) : string =
  begin match t.t with
    | TInt       -> "int"
    | TFlt       -> "flt"
    | TChar      -> "char"
    | TBool      -> "bool"
    | TRef r     -> print_rty (ofnode r t)
    | TNullRef r -> Printf.sprintf "%s?" (print_rty (ofnode r t))
  end
and print_retty (t : retty node) : string =
  begin match t.t with
    | Void  -> "void"
    | Ret r -> print_ty (ofnode r t)
  end
and print_rty (r : rty node) : string =
  begin match r.t with
    | TStr   -> "string"
    | TArr t -> Printf.sprintf "[%s]" (print_ty @@ ofnode t r)
    | TFun (args,rt) ->
        Printf.sprintf "(%s) -> %s" (String.concat ", " (List.map (fun arg -> print_ty (ofnode arg r)) args)) (print_retty (ofnode rt r))
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

let rec print_exp (e : exp node) : string =
  begin match e.t with
    | Id        s           -> s
    | LitInt    i           -> Printf.sprintf "%Ld"    i
    | LitFlt    f           -> Printf.sprintf "%f"     f
    | LitChar   c           -> Printf.sprintf "'%c'"   c
    | LitBool   b           -> Printf.sprintf "%B"     b
    | LitStr    s           -> Printf.sprintf "\"%s\"" (String.escaped s)
    | LitArr    ls          -> Printf.sprintf "[%s]" (String.concat ", " (List.map print_exp ls))
    | Deref     e           -> Printf.sprintf "assert %s" (print_exp e)
    | EmptyList None        -> "[]"
    | EmptyList (Some t)    -> Printf.sprintf "([] of %s)" (print_ty t)
    | RangeList (s,i1,i2,e) -> Printf.sprintf "[%s%s%s]" (print_exp s) (print_incl (i1,i2)) (print_exp e)
    | ListComp  (e,vs,c)    -> Printf.sprintf "[%s : %s : %s]" (print_exp e) (String.concat ", " (List.map (fun (id,e) -> Printf.sprintf "%s in %s" id (print_exp e)) vs)) (print_exp c)
    | Ternary   (c,e1,e2)   -> Printf.sprintf "(?%s -> %s : %s)" (print_exp c) (print_exp e1) (print_exp e2)
    | Null      None        -> "null"
    | Null      (Some t)    -> Printf.sprintf "(null of %s)" (print_rty t)
    | Sprintf   (t,s,es)    -> Printf.sprintf "%sprintf(\"%s\"%s)" (if t = Sprintf then "s" else "") (String.escaped s.t) (String.concat "" (List.map (fun e -> ", " ^ print_exp e) es))
    | Bop       (o,l,r)     -> Printf.sprintf "(%s %s %s)" (print_exp l) (List.assoc o bop_string) (print_exp r)
    | Uop       (o,e)       -> Printf.sprintf "(%s%s)" (List.assoc o uop_string) (print_exp e)
    | Cmps      (e,ls)      -> Printf.sprintf "(%s%s)" (print_exp e) (List.fold_left (fun s (o,e) -> Printf.sprintf "%s %s %s" s (List.assoc o cmp_string) (print_exp e)) "" ls)
    | FApp      (f,ls)      -> Printf.sprintf "%s(%s)" (print_exp f) (String.concat ", " (List.map (fun e -> match e.t with | None -> "_" | Some e' -> print_exp (ofnode e' e)) ls))
    | Subscript (l,r)       -> Printf.sprintf "%s[%s]" (print_exp l) (print_exp r)
    | Proj      (e,s)       -> Printf.sprintf "%s.%s" (print_exp e) s.t
  end

let rec print_stmt (indent : int) (s : stmt node) : string =
  let ind = String.make (2*indent) ' ' in
  begin match s.t with
    | VDecl   (id,Const,None,e)   -> Printf.sprintf "%slet %s := %s\n"              ind id (print_exp e)
    | VDecl   (id,Const,Some t,e) -> Printf.sprintf "%slet %s:%s := %s\n"           ind id (print_ty t) (print_exp e)
    | VDecl   (id,Mut,None,e)     -> Printf.sprintf "%smut %s := %s\n"              ind id (print_exp e)
    | VDecl   (id,Mut,Some t,e)   -> Printf.sprintf "%smut %s:%s := %s\n"           ind id (print_ty t) (print_exp e)
    | Assert  e                   -> Printf.sprintf "%sassert %s\n"                 ind (print_exp e)
    | Assn    (l,r)               -> Printf.sprintf "%s%s := %s\n"                  ind (print_exp l) (print_exp r)
    | Expr    e                   -> Printf.sprintf "%s%s\n"                        ind (print_exp e)
    | If      (c,t,n)             -> Printf.sprintf "%sif %s\n%s%selse\n%s"         ind (print_exp c) (print_block (indent+1) t) ind (print_block (indent+1) n)
    | Denull  (id,e,t,n)          -> Printf.sprintf "%sdenull %s := %s\n%s%selse\n%s" ind id (print_exp e) (print_block (indent+1) t) ind (print_block (indent+1) n)
    | While   (c,b)               -> Printf.sprintf "%swhile %s\n%s"                ind (print_exp c) (print_block (indent+1) b)
    | DoWhile (c,b)               -> Printf.sprintf "%sdo\n%s\n%swhile %s\n"        ind (print_block (indent+1) b) ind (print_exp c)
    | For     (i,s,i1,i2,e,b)     -> Printf.sprintf "%sfor %s := %s %s %s\n%s"      ind i (print_exp s) (print_incl (i1,i2)) (print_exp e) (print_block (indent+1) b)
    | ForIn   (i,l,b)             -> Printf.sprintf "%sfor %s in %s\n%s"            ind i (print_exp l) (print_block (indent+1) b)
    | Break                       -> Printf.sprintf "%sbreak\n"                     ind
    | Continue                    -> Printf.sprintf "%scontinue\n"                  ind
    | Return  None                -> Printf.sprintf "%sreturn\n"                    ind
    | Return  (Some e)            -> Printf.sprintf "%sreturn %s\n"                 ind (print_exp e)
  end
and print_block (indent : int) (b : stmt node list) : string =
  (String.concat "" (List.map (print_stmt (indent+1)) b))

let print_gstmt (gs : gstmt node) : string =
  begin match gs.t with
    | GVDecl (id,Const,None,e)   -> Printf.sprintf "global %s := %s\n\n" id (print_exp e)
    | GVDecl (id,Const,Some t,e) -> Printf.sprintf "global %s:%s := %s\n\n" id (print_ty t) (print_exp e)
    | GVDecl (id,Mut,None,e)     -> Printf.sprintf "global mut %s := %s\n\n" id (print_exp e)
    | GVDecl (id,Mut,Some t,e)   -> Printf.sprintf "global mut %s:%s := %s\n\n" id (print_ty t) (print_exp e)
    | GFDecl (id,[],rt,b)        -> Printf.sprintf "fn %s => %s\n%s\n" id (print_retty rt) (print_block 1 b)
    | GFDecl (id,args,rt,b)      -> Printf.sprintf "fn %s : %s => %s\n%s\n" id (String.concat ", " (List.map (fun (s,t) -> Printf.sprintf "%s:%s" s (print_ty t)) args)) (print_retty rt) (print_block 1 b)
    | Module m                   -> Printf.sprintf "module %s\n\n\n" m
    | GNFDecl (id,ats,rt)        -> Printf.sprintf "native fn %s%s %s => %s\n" id (if ats=[] then "" else ":") (String.concat ", " (List.map print_ty ats)) (print_retty rt)
    | GNVDecl (id,t)             -> Printf.sprintf "native %s: %s\n" id (print_ty t)
    | GNTDecl id                 -> Printf.sprintf "native type %s\n" id
  end

let print_program (prog : program) : string =
  String.concat "" @@ List.map print_gstmt prog