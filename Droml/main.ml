open Common
open Token
open Lexer
open Parser
open Ast
open Typechecker
open Translator
open Ll
  
let () =
  let filenames = List.tl @@ Array.to_list Sys.argv in
  let programs = List.map (String.cat "\n") (List.map readall filenames) in
  let lexes =
    try List.map Lexer.lex programs
    with Lexer.LexError {t=m;start=s;length=l} ->
      (Printf.printf "Error at [%d-%d]: '%s'.\n" s (s+l) m;
       Stdlib.failwith "Lexer Error. Aborting.")
  in
  (* let _ = List.iter (fun t -> Printf.printf "%s\n" (Token.print_token t.t)) (List.concat lexes), Printf.printf "\n" in *)
  let parses =
    try List.map Parser.parse lexes
    with Parser.ParseError {t=m;start=s;length=l} ->
      (Printf.printf "Error at [%d-%d] -> '%s'.\n" s (s+l) m;
       Stdlib.failwith "Parser Error. Aborting.")
  in
  let program = List.fold_left (@) [] parses in
  let () = Printf.printf "\n%s\n" (Ast.print_program program) in

  let annt_prog =
    try TypeChecker.check_program program
    with TypeChecker.TypeError ({t=m;start=s;length=l}) ->
      (Printf.printf "Error at [%d-%d]: '%s'.\n" s (s+l) m;
       Stdlib.failwith "Type Error. Aborting.")
  in
  (* Printf.printf "\n%s\n" (Ast.print_program program); *)
  
  let llstring = Translator.cmp_to_llvm annt_prog in
  let llfile = Stdlib.open_out "Out.ll" in
  Printf.fprintf llfile "%s\n" llstring;
  (* Printf.printf "\n%s\n" (Ast.print_program program); *)
  (*
  Printf.printf "\n%s\n" llstring;
  *)
  Stdlib.close_out llfile;
  
  

  (*
    -v to see full linker output
    -lm to add math.h library because apparently that is necessary
   *)
  let _ = Sys.command "clang -S Out.ll -O3" in
  let _ = Sys.command "clang -o a.out Out.ll cutils/*.o -L. ./cutils/gc.so -lstdc++ -lm -O3" in
  ()