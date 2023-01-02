type 'a node = { t:'a ; start:int ; length:int }

let ofnode (v : 'a) (n : 'b node) : 'a node =
  { t = v ; start = n.start ; length = n.length }

let rec readall (filename : string) : string =
  let rec aux (ic : Stdlib.in_channel) : string list =
    try
      let line = Stdlib.input_line ic in
      line :: aux ic
    with End_of_file ->
      close_in_noerr ic; []
  in
  String.concat "\n" @@ aux (open_in filename)

let default_module_name : string = "Default"

let filename_to_module (filename : string) : string =
  let filename_regex = "[A-Za-z0-9]+" in
  if Str.string_match (Str.regexp filename_regex) filename 0 then
    Str.matched_string filename
  else
    default_module_name