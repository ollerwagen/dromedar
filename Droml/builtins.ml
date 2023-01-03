open Ast
open Ll

let builtins : (string * string * ty * operand) list =
  [ "Str", "of_int", TRef (TFun ([TInt], Ret (TRef TStr))), Gid "_Str$of_int"
  ; "Str", "of_flt", TRef (TFun ([TFlt], Ret (TRef TStr))), Gid "_Str$of_flt"
    
  ; "IO", "print_int",  TRef (TFun ([TInt], Void)),      Gid "_IO$print_int"
  ; "IO", "print_flt",  TRef (TFun ([TFlt], Void)),      Gid "_IO$print_flt"
  ; "IO", "print_str",  TRef (TFun ([TRef TStr], Void)), Gid "_IO$print_str"
  ; "IO", "print_char", TRef (TFun ([TChar], Void)),     Gid "_IO$print_char"
  ; "IO", "print_bool", TRef (TFun ([TBool], Void)),     Gid "_IO$print_bool"

  ; "File", "readall", TRef (TFun ([TRef TStr], Ret (TRef (TArr (TRef TStr))))), Gid "_File$readall"
  
  ; "Math", "sin", TRef (TFun ([TFlt], Ret TFlt)), Gid "_Math$sin"
  ; "Math", "cos", TRef (TFun ([TFlt], Ret TFlt)), Gid "_Math$cos"
  ; "Math", "tan", TRef (TFun ([TFlt], Ret TFlt)), Gid "_Math$tan"
  ; "Math", "e",   TFlt,                           Gid "_Math$e"
  ; "Math", "pi",  TFlt,                           Gid "_Math$pi"
  ]