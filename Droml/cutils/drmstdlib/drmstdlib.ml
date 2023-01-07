module Drmstdlib = struct

  type llty =
    | Void
    | VariadicDots
    | Namedt of string
    | Struct of llty list
    | Func of llty list * llty
    | Array of int64 * llty
    | I1
    | I8
    | I64
    | Double
    | Ptr of llty

  let rec print_llty (t:llty) : string =
    begin match t with
      | Void         -> "void"
      | VariadicDots -> "..."
      | Namedt s     -> "%%%s"
      | Struct ts    -> Printf.sprintf "{%s}" (String.concat ", " (List.map print_llty ts))
      | Func   (a,r) -> Printf.sprintf "%s(%s)" (print_llty r) (String.concat ", " (List.map print_llty a))
      | Array  (s,t) -> Printf.sprintf "[%Ld x %s]" s (print_llty t)
      | I1           -> "i1"
      | I8           -> "i8"
      | I64          -> "i64"
      | Double       -> "double"
      | Ptr    t     -> Printf.sprintf "%s*" (print_llty t)
    end

  let strty = Ptr (Struct [ I64 ; Ptr (Array (0L, I8))])

  let stdlib : (string * string * llty) list =
    [ "Str",   "of_int",     Func([Ptr I64 ; I64],    strty)
    ; "Str",   "of_flt",     Func([Ptr I64 ; Double], strty)
    ; "IO",    "print_int",  Func([Ptr I64 ; I64],    Void)
    ; "IO",    "print_flt",  Func([Ptr I64 ; Double], Void)
    ; "IO",    "print_str",  Func([Ptr I64 ; strty],  Void)
    ; "IO",    "print_char", Func([Ptr I64 ; I8],     Void)
    ; "IO",    "print_bool", Func([Ptr I64 ; I1],     Void)
    ; "File",  "readall",    Func([Ptr I64 ; strty],  Ptr (Struct [ I64 ; Ptr (Array (0L, strty)) ]))
    ; "Math",  "sin",        Func([Ptr I64 ; Double], Double)
    ; "Math",  "cos",        Func([Ptr I64 ; Double], Double)
    ; "Math",  "tan",        Func([Ptr I64 ; Double], Double)
    ; "Math",  "e",          Double
    ; "Math",  "pi",         Double
    ; "Regex", "compile",    Func([Ptr I64 ; strty],          Ptr I8)
    ; "Regex", "matches",    Func([Ptr I64 ; Ptr I8 ; strty], I1)
    ]

end

let () =
  let oc = Stdlib.open_out "cutils/drmstdlib/drmstdlib.ll" in
  List.iter
    Drmstdlib.(fun (m,id,t) ->
        begin match t with
          | Func (a, rt) ->
              Printf.fprintf oc "declare %s @__%s$%s(%s)\n@__%s$%s$ = global { %s* } { %s* @__%s$%s }\n@_%s$%s = global { %s* }* @__%s$%s$\n\n"
                (print_llty rt) m id (String.concat ", " (List.map print_llty a))
                m id (print_llty t) (print_llty t) m id
                m id (print_llty t) m id
          | t ->
              Printf.fprintf oc "@_%s$%s = external global %s\n\n"
                m id (print_llty t)
        end
    )
    Drmstdlib.stdlib