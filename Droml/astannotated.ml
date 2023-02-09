open Ast

type exp' =
  | Id        of string
  | LitInt    of Token.tint
  | LitFlt    of Token.tflt
  | LitChar   of Token.tchar
  | LitBool   of Token.tbool
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
  | ParFApp   of annt_exp * annt_exp option list
  | Subscript of annt_exp * annt_exp
  | Proj      of annt_exp * string
and annt_exp = exp' * ty

type annt_stmt =
  | VDecl   of string * mutability * ty option * annt_exp
  | Assert  of annt_exp * string
  | Assn    of annt_exp * annt_exp
  | Expr    of exp' * ty option
  | If      of annt_exp * annt_stmt list * annt_stmt list
  | Denull  of string * annt_exp * annt_stmt list * annt_stmt list
  | While   of annt_exp * annt_stmt list
  | DoWhile of annt_exp * annt_stmt list
  | For     of string * annt_exp * inclusion * inclusion * annt_exp * annt_stmt list
  | ForIn   of string * annt_exp * annt_stmt list
  | Break
  | Continue
  | Return  of annt_exp option

type annt_gstmt =
  | GVDecl  of string * mutability * ty option * annt_exp
  | GFDecl  of string * (string * ty) list * retty * annt_stmt list
  | GTDecl  of string * (string * ty) list * (string * ((string * ty) list * retty * annt_stmt list)) list
  | GNFDecl of string * ty list * retty
  | GNVDecl of string * ty
  | GNTDecl of string

type annt_program = annt_gstmt list
