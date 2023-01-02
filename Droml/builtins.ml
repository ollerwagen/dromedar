open Ast
open Ll

let builtins : (string * string * ty) list =
  [ "IO", "print_int",  TRef (TFun ([TInt], Void))
  ; "IO", "print_flt",  TRef (TFun ([TFlt], Void))
  ; "IO", "print_str",  TRef (TFun ([TRef TStr], Void))
  ; "IO", "print_char", TRef (TFun ([TChar], Void))
  ; "IO", "println",    TRef (TFun ([], Void))

  ; "Str", "int_to_str", TRef (TFun ([TInt], Ret (TRef TStr)))
  ]