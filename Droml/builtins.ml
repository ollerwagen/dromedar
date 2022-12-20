open Ast
open Ll

let builtins : (string * ty) list =
  [ "print_int", TRef (TFun ([TInt], Void))
  ; "print_flt", TRef (TFun ([TFlt], Void))
  ]