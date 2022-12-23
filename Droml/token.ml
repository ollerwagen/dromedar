type tint  = int64
type tflt  = float
type tchar = char
type tbool = bool

type op =
  | Bang
  | Starstar
  | Star
  | Plus
  | Dash
  | LShift
  | RShift
  | AShift
  | Bitand
  | Bitxor
  | Bitor
  | Logand
  | Logxor
  | Logor
  | Equal
  | NotEqual
  | Greater
  | Less
  | GreaterEq
  | LessEq

type token =
  | LInt  of tint
  | LFlt  of tflt
  | LChar of tchar
  | LBool of tbool
  | LStr  of string
  | Id    of string
  | Op    of op
  | Assign
  | Colon
  | Comma
  | Arrow
  | LParen
  | RParen
  | LBrack
  | RBrack
  | QuestionMark
  | Dots
  | DotsPipe
  | EOF
  | Whitespace of int
  | KGlobal
  | KFn
  | KLet
  | KMut
  | KInt
  | KFlt
  | KChar
  | KBool
  | KString
  | KVoid
  | KNull
  | KOf
  | KIf
  | KElif
  | KElse
  | KWhile
  | KDo
  | KFor
  | KReturn

let print_token (t:token) : string =
  let opstrings : (op * string) list = [
      Bang,      "!"
    ; Starstar,  "**"
    ; Star,      "*"
    ; Plus,      "+"
    ; Dash,      "-"
    ; LShift,    "<<"
    ; RShift,    ">>"
    ; AShift,    ">>>"
    ; Bitand,    "&"
    ; Bitxor,    "^"
    ; Bitor,     "|"
    ; Logand,    "&&"
    ; Logxor,    "^^"
    ; Logor,     "||"
    ; Equal,     "="
    ; NotEqual,  "!="
    ; Greater,   ">"
    ; Less,      "<"
    ; GreaterEq, ">="
    ; LessEq,    "<="
    ] in
  begin match t with
    | LInt  i      -> Printf.sprintf "[Int %s]"  (Int64.to_string i)
    | LFlt  f      -> Printf.sprintf "[Flt %f]"  f
    | LChar c      -> Printf.sprintf "[Char %c]" c
    | LBool b      -> Printf.sprintf "[Bool %B]" b
    | Id    s      -> Printf.sprintf "[Id %s]"   s
    | LStr  s      -> Printf.sprintf "[Str %s]"  (String.escaped s)
    | Op    o      -> Printf.sprintf "[Op %s]"   (List.assoc o opstrings)
    | Assign       -> "[Assign]"
    | Colon        -> "[Colon]"
    | Comma        -> "[Comma]"
    | Arrow        -> "[Arrow]"
    | LParen       -> "[LParen]"
    | RParen       -> "[RParen]"
    | LBrack       -> "[LBrack]"
    | RBrack       -> "[RBrack]"
    | QuestionMark -> "[QuestionMark]"
    | Dots         -> "[Dots]"
    | DotsPipe     -> "[DotsPipe]"
    | EOF          -> "[EOF]"
    | Whitespace i -> Printf.sprintf "[Whitespace %d]" i
    | KGlobal      -> "[global]"
    | KFn          -> "[fn]"
    | KLet         -> "[let]"
    | KMut         -> "[mut]"
    | KInt         -> "[int]"
    | KFlt         -> "[flt]"
    | KChar        -> "[char]"
    | KBool        -> "[bool]"
    | KString      -> "[string]"
    | KVoid        -> "[void]"
    | KNull        -> "[null]"
    | KOf          -> "[of]"
    | KIf          -> "[if]"
    | KElif        -> "[elif]"
    | KElse        -> "[else]"
    | KWhile       -> "[while]"
    | KDo          -> "[do]"
    | KFor         -> "[for]"
    | KReturn      -> "[return]"
  end