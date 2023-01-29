type tint  = int64
type tflt  = float
type tchar = char
type tbool = bool

type op =
  | Bang
  | Starstar
  | Star
  | Slash
  | Percent
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
  | RefEqual
  | RefNotEqual

type token =
  | LInt  of tint
  | LFlt  of tflt
  | LChar of tchar
  | LBool of tbool
  | LStr  of string
  | Id    of string
  | Op    of op
  | Assign
  | Dot
  | Colon
  | Comma
  | Arrow
  | DoubleArrow
  | Underscore
  | LParen
  | RParen
  | LBrack
  | RBrack
  | QuestionMark
  | Dots
  | DotsPipe
  | PipeDots
  | PipeDotPipe
  | EOF
  | Whitespace of (int, string) Either.t
  | Semicolon
  | KModule
  | KNative
  | KUsing
  | KGlobal
  | KFn
  | KLet
  | KMut
  | KType
  | KInt
  | KFlt
  | KChar
  | KBool
  | KString
  | KVoid
  | KNull
  | KPrintf
  | KSprintf
  | KAssert
  | KOf
  | KIn
  | KIf
  | KElif
  | KElse
  | KDenull
  | KWhile
  | KDo
  | KFor
  | KRepeat
  | KBreak
  | KContinue
  | KReturn

let print_token (t:token) : string =
  let opstrings : (op * string) list = [
      Bang,        "!"
    ; Starstar,    "**"
    ; Star,        "*"
    ; Slash,       "/"
    ; Percent,     "%"
    ; Plus,        "+"
    ; Dash,        "-"
    ; LShift,      "<<"
    ; RShift,      ">>"
    ; AShift,      ">>>"
    ; Bitand,      "&"
    ; Bitxor,      "^"
    ; Bitor,       "|"
    ; Logand,      "&&"
    ; Logxor,      "^^"
    ; Logor,       "||"
    ; Equal,       "="
    ; NotEqual,    "!="
    ; Greater,     ">"
    ; Less,        "<"
    ; GreaterEq,   ">="
    ; LessEq,      "<="
    ; RefEqual,    "=="
    ; RefNotEqual, "!=="
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
    | Dot          -> "[Dot]"
    | Colon        -> "[Colon]"
    | Comma        -> "[Comma]"
    | Arrow        -> "[Arrow]"
    | DoubleArrow  -> "[DoubleArrow]"
    | Underscore   -> "[Underscore]"
    | LParen       -> "[LParen]"
    | RParen       -> "[RParen]"
    | LBrack       -> "[LBrack]"
    | RBrack       -> "[RBrack]"
    | QuestionMark -> "[QuestionMark]"
    | Dots         -> "[Dots]"
    | DotsPipe     -> "[DotsPipe]"
    | PipeDots     -> "[PipeDots]"
    | PipeDotPipe  -> "[PipeDotPipe]"
    | EOF          -> "[EOF]"
    | KModule      -> "[module]"
    | KNative      -> "[native]"
    | KUsing       -> "[using]"
    | KGlobal      -> "[global]"
    | KFn          -> "[fn]"
    | KLet         -> "[let]"
    | KMut         -> "[mut]"
    | KType        -> "[type]"
    | KInt         -> "[int]"
    | KFlt         -> "[flt]"
    | KChar        -> "[char]"
    | KBool        -> "[bool]"
    | KString      -> "[string]"
    | KVoid        -> "[void]"
    | KNull        -> "[null]"
    | KOf          -> "[of]"
    | KIn          -> "[in]"
    | KPrintf      -> "[printf]"
    | KSprintf     -> "[sprintf]"
    | KAssert      -> "[assert]"
    | KIf          -> "[if]"
    | KElif        -> "[elif]"
    | KElse        -> "[else]"
    | KDenull      -> "[denull]"
    | KWhile       -> "[while]"
    | KDo          -> "[do]"
    | KFor         -> "[for]"
    | KRepeat      -> "[repeat]"
    | KBreak       -> "[break]"
    | KContinue    -> "[continue]"
    | KReturn      -> "[return]"
    | Whitespace (Left i) -> Printf.sprintf "[Whitespace %d]" i
    | Whitespace (Right _) -> Stdlib.failwith "cannot print pure-string indent, should resolve first"
    | Semicolon    -> "[Semicolon]"
  end