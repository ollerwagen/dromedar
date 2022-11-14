type token =
  | LitInt of int64
  | LitFlt of double
  | LitChar of char
  | LitBool of bool
  | Identifier of string
  | Dash
  | Bang
  | Star
  | Plus
  | LParen
  | RParen
  | EOF
  | True
  | False

type fulltoken = {t:token; lexeme:string; int:start; int:length}
