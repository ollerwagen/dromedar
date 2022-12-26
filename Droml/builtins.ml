open Ast
open Ll

let builtins : (string * ty) list =
  [ "print_int", TRef (TFun ([TInt], Void))
  ; "print_flt", TRef (TFun ([TFlt], Void))
  ; "print_str", TRef (TFun ([TRef TStr], Void))
  ; "println",   TRef (TFun ([], Void))

  ; "int_to_str", TRef (TFun ([TInt], Ret (TRef TStr)))
  ]