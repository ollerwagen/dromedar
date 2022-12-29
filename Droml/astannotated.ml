open Ast

type exp' =
  | Id        of string
  | LitInt    of Token.tint
  | LitFlt    of Token.tflt
  | LitChar   of Token.tchar
  | LitBool   of Token.tbool
  | LitStr    of string
  | LitArr    of annt_exp list
  | EmptyList of ty
  | RangeList of annt_exp * inclusion * inclusion * annt_exp
  | Null      of rty
  | Sprintf   of formatstr * string * annt_exp list
  | Bop       of bop * annt_exp * annt_exp
  | Uop       of uop * annt_exp
  | Cmps      of annt_exp * (cmpop * annt_exp) list
  | FApp      of annt_exp * annt_exp list
  | Subscript of annt_exp * annt_exp
and annt_exp = exp' * ty

type annt_stmt =
  | VDecl   of string * mutability * ty option * annt_exp
  | Assn    of annt_exp * annt_exp
  | Expr    of exp' * ty option
  | If      of annt_exp * annt_stmt list * annt_stmt list
  | Denull  of string * annt_exp * annt_stmt list * annt_stmt list
  | While   of annt_exp * annt_stmt list
  | DoWhile of annt_exp * annt_stmt list
  | For     of string * annt_exp * inclusion * inclusion * annt_exp * annt_stmt list
  | Return  of annt_exp option

type annt_gstmt =
  | GVDecl of string * mutability * ty option * annt_exp
  | GFDecl of string * (string * ty) list * retty * annt_stmt list

type annt_program = annt_gstmt list
