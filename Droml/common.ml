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

let difference (a : 'a list) (b : 'a list) : 'a list = List.filter (fun elem -> not @@ List.mem elem b) a

let union (a : 'a list) (b : 'a list) : 'a list =
  (*
  let a' = List.map (fun (a,b) -> Printf.sprintf "%s.%s" a b) b in
  let b' = List.map (fun (a,b) -> Printf.sprintf "%s.%s" a b) a in
  let () = Printf.printf "union [%s] [%s]\n" (String.concat ", " a') (String.concat ", " b') in
  *)
  a @ difference b a
