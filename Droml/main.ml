open Common
open Token
open Lexer
open Parser
open Ast
open Typechecker
open Translator
open Ll

open Templateresolver

let read_lines (filename : string) : string list =
  List.filter (fun s -> String.length s > 0) @@ String.split_on_char '\n' (readall filename)

(*
let () =
  let ets = [ TRef (TFun ([ TTempl (true, "a") ], Ret (TTempl (true, "a")))) ] in
  let pts = [ TRef (TFun ([ TRef TStr ], Ret (TRef TStr))) ] in
  let res = TemplateResolver.resolve_templates @@ List.combine pts ets in
  Printf.printf "[%s]\n" @@ String.concat ", " @@ List.map (fun (id,t) -> Printf.sprintf "<%s> -> (%s)" id (Ast.print_ty {t=t;start=0;length=0})) res
*)


let () =
  let argv = List.tl @@ Array.to_list Sys.argv in

  let seelexoutput,seeparseoutput,seellvmoutput,seeasoutput,compiletollvm,inputfiles,inputllfiles,inputcfiles,inputobjfiles,outputfilename,mustbefalse =
    List.fold_left
      (fun (lx,p,ll,s,c,is,ills,ics,ios,o,iso) arg ->
        if iso then
          lx,p,ll,s,c,is,ills,ics,ios,Some arg,false
        else
          begin match arg with
            | "-l"  -> true,p,ll,s,c,is,ills,ics,ios,o,false
            | "-p"  -> lx,true,ll,s,c,is,ills,ics,ios,o,false
            | "-ll" -> lx,p,true,s,c,is,ills,ics,ios,o,false
            | "-s"  -> lx,p,ll,true,c,is,ills,ics,ios,o,false
            | "-c"  -> lx,p,ll,s,true,is,ills,ics,ios,o,false
            | "-o"  ->
                if o <> None then
                  Stdlib.failwith "cannot specify multiple output files"
                else
                  lx,p,ll,s,c,is,ills,ics,ios,o,true
            | name -> 
                if String.ends_with ~suffix:".drm" name then
                  lx,p,ll,s,c,is@[name],ills,ics,ios,o,false
                else if String.ends_with ~suffix:".ll" name then
                  lx,p,ll,s,c,is,ills@[name],ics,ios,o,false
                else if String.ends_with ~suffix:".c" name then
                  lx,p,ll,s,c,is,ills,ics@[name],ios,o,false
                else if String.ends_with ~suffix:".o" name then
                  lx,p,ll,s,c,is,ills,ics,ios@[name],o,false
                else
                  Stdlib.failwith "have to specify a .drm, .ll, or .c input file"
          end
      )
      (false,false,false,false,false,[],[],[],[],None,false)
      argv in

  let outputfilename =
    begin match outputfilename with
      | None      -> "a.out"
      | Some name -> name
    end in

  let doanycompilation = inputfiles <> [] in

  let () = if mustbefalse then Stdlib.failwith "must specify output file after -o" else () in

  let () =
    if seeasoutput && compiletollvm then
      Stdlib.failwith "cannot use -s and -c at the same time"
    else () in

  let () =
    if (seeasoutput || compiletollvm) && (inputllfiles <> [] || inputcfiles <> []) then
      Stdlib.failwith "if you do not compile to an executable, you cannot give .c or .ll input files"
    else if (seelexoutput || seeparseoutput || seellvmoutput || seeasoutput || compiletollvm) && not doanycompilation then
      Stdlib.failwith "cannot see debug output if the compiler doesn't have to do anything with .drm files"
    else () in

  let () =

    if doanycompilation then

      let filenames = (read_lines "LibLists/drmlibs.txt") @ inputfiles in
      let programs = List.map (String.cat "\n") (List.map readall filenames) in

      let lexes =
        try List.map Lexer.lex programs
        with Lexer.LexError {t=m;start=s;length=l} ->
          (Printf.printf "Error at [%d-%d]: '%s'.\n" s (s+l) m;
          Stdlib.failwith "Lexer Error. Aborting.")
      in

      let () =
        if seelexoutput then
          List.iter (fun t -> Printf.printf "%s\n" (Token.print_token t.t)) (List.concat lexes)
        else () in

      let parses =
        try List.map Parser.parse lexes
        with Parser.ParseError {t=m;start=s;length=l} ->
          (Printf.printf "Error at [%d-%d] -> '%s'.\n" s (s+l) m;
          Stdlib.failwith "Parser Error. Aborting.")
      in
      let parses' =
        List.map2 (fun p filename -> { t = Module (filename_to_module filename) ; start = 0 ; length = 0 } :: p) parses filenames in
      let program = List.fold_left (@) [] parses' in

      let () =
        if seeparseoutput then
          Printf.printf "\n%s\n" (Ast.print_program program)
        else () in

      let annt_prog, main_id =
        try TypeChecker.check_program program
        with TypeChecker.TypeError ({t=m;start=s;length=l}) ->
          (Printf.printf "Error at [%d-%d]: '%s'.\n" s (s+l) m;
          Stdlib.failwith "Type Error. Aborting.")
      in
      
      let llstring = Translator.cmp_to_llvm annt_prog main_id in

      let () =
        if seellvmoutput then
          Printf.printf "%s\n" llstring
        else () in

      let () =
        if compiletollvm then
          let llfile = Stdlib.open_out outputfilename in
          let () = Printf.fprintf llfile "%s\n" llstring in
          Stdlib.close_out llfile
        else () in

      let _ =
        if seeasoutput then
          let llfile = Stdlib.open_out (outputfilename ^ ".ll") in
          let () = Printf.fprintf llfile "%s\n" llstring in
          let () = Stdlib.close_out llfile in
          Sys.command (Printf.sprintf "clang -S %s.ll -o %s -O3; rm %s.ll" outputfilename outputfilename outputfilename)
        else 0 in

      let () =
        if not compiletollvm && not seeasoutput then
          let llfile = Stdlib.open_out (outputfilename ^ ".ll") in
          let () = Printf.fprintf llfile "%s\n" llstring in
          Stdlib.close_out llfile
        else () in

      ()

    else () in
  
  let _ =
    if not compiletollvm && not seeasoutput then
      let cppliblinks = String.concat " " (List.map (fun l -> Printf.sprintf "-L. ./%s" l) (read_lines "LibLists/cpplibs.txt")) in
      Sys.command (Printf.sprintf "clang -o %s %s %s obj/*.o %s -lstdc++ -lm -O0 %s"
        outputfilename
        (String.concat " " (inputcfiles @ inputllfiles @ inputobjfiles))
        (if doanycompilation then outputfilename ^ ".ll" else "")
        cppliblinks
        (if doanycompilation then Printf.sprintf "; rm %s.ll" outputfilename else "")
      )
    else 0 in

  ()

  (*
    -v to see full linker output
    -lm to add math.h library because apparently that is necessary
   *)