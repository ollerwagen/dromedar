type 'a node = { t:'a ; start:int ; length:int }

let ofnode (v : 'a) (n : 'b node) : 'a node =
  { t = v ; start = n.start ; length = n.length }

let rec drop (n : int) (l : 'a list) : 'a list =
  begin match n with
    | 0 -> l
    | n -> drop (n-1) (List.tl l)
  end

let rec take (n : int) (l : 'a list) : 'a list =
  begin match n with
    | 0 -> []
    | n -> List.hd l :: take (n-1) (List.tl l)
  end

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

let identity (x:'a) : 'a = x

let intersect_single (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.fold_left (fun i x -> if List.mem x l2 then x::i else i) [] l1

let intersect (l : 'a list list) : 'a list =
  begin match l with
    | []    -> []
    | l::ls -> List.fold_left intersect_single l ls
  end

let rec crossp (l : 'a list list) : 'a list list = 
  let rec aux acc l1 l2 =
    begin match l1, l2 with
      | [], _ | _, [] -> acc
      | h1::t1, h2::t2 -> 
          let acc = (h1::h2)::acc in
          let acc = (aux acc t1 l2) in
          aux acc [h1] t2
    end
  in
  begin match l with
    | [] -> []
    | [l1] -> List.map (fun x -> [x]) l1
    | l1::tl ->
        let tail_product = crossp tl in
        aux [] l1 tail_product
  end

let rec crossp_single (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  begin match l1 with
    | []    -> []
    | l::ls -> List.map (fun l2e -> l,l2e) l2 @ crossp_single ls l2
  end

let rec alldistinct (l : 'a list) : bool =
  begin match l with
    | []    -> true
    | x::xs -> if List.mem x xs then false else alldistinct xs
  end