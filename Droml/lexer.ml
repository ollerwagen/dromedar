open Common
open Token

module Lexer = struct

  let tabsize : int =
    let lines = String.split_on_char '\n' @@ readall "droml.config" in
    begin match List.find_opt (String.starts_with ~prefix:"tabsize=") lines with
      | None   -> Stdlib.failwith "tabsize=<num> line not found in droml.config file"
      | Some s ->
          let numstring = String.sub s (String.length "tabsize=") (String.length s - String.length "tabsize=") in
          begin match Stdlib.int_of_string_opt numstring with
            | None   -> Stdlib.failwith "tabsize=<num> should be a number"
            | Some x -> x
          end
    end

  exception LexError of string node

  let simple_regex_strings : (string * token) list =
    [ "!",   Op Bang
    ; "**",  Op Starstar
    ; "*",   Op Star    
    ; "/",   Op Slash
    ; "%",   Op Percent  
    ; "+",   Op Plus      
    ; "-",   Op Dash     
    ; "<<",  Op LShift    
    ; ">>",  Op RShift    
    ; ">>>", Op AShift    
    ; "&",   Op Bitand    
    ; "^",   Op Bitxor
    ; "|",   Op Bitor     
    ; "&&",  Op Logand    
    ; "^^",  Op Logxor
    ; "||",  Op Logor
    ; "=",   Op Equal     
    ; "!=",  Op NotEqual  
    ; ">",   Op Greater   
    ; "<",   Op Less    
    ; ">=",  Op GreaterEq 
    ; "<=",  Op LessEq  
    ; "==",  Op RefEqual
    ; "!==", Op RefNotEqual
    ; ":=",  Assign
    ; ".",   Dot
    ; ":",   Colon
    ; ",",   Comma
    ; "->",  Arrow
    ; "=>",  DoubleArrow
    ; "_",   Underscore
    ; "(",   LParen
    ; ")",   RParen
    ; "[",   LBrack
    ; "]",   RBrack
    ; "?",   QuestionMark
    ; "...", Dots
    ; "..|", DotsPipe
    ; "|..", PipeDots
    ; "|.|", PipeDotPipe
    ; ";",   Semicolon
    ]

  let simple_regexes : (Str.regexp * token) list =
    let f ((r1,_) : (string * token)) ((r2,_) : (string * token)) : int =
      if String.length r1 > String.length r2 then -1 else 1
    in
    List.map (fun (s,t) -> Str.regexp_string s, t) (List.sort f simple_regex_strings)
  
  let keywords : (string * token) list = [
      "module",   KModule
    ; "native",   KNative
    ; "global",   KGlobal
    ; "fn",       KFn
    ; "let",      KLet
    ; "mut",      KMut
    ; "type",     KType
    ; "int",      KInt
    ; "flt",      KFlt
    ; "char",     KChar
    ; "bool",     KBool
    ; "string",   KString
    ; "void",     KVoid
    ; "null",     KNull
    ; "of",       KOf
    ; "in",       KIn
    ; "printf",   KPrintf
    ; "sprintf",  KSprintf
    ; "assert",   KAssert
    ; "if",       KIf
    ; "elif",     KElif
    ; "else",     KElse
    ; "denull",   KDenull
    ; "while",    KWhile
    ; "for",      KFor
    ; "do",       KDo
    ; "break",    KBreak
    ; "continue", KContinue
    ; "return",   KReturn
    ; "true",     LBool true
    ; "false",    LBool false
    ]
  
  type other_regexes = | Id | Flt | Int | Char | CharEscape | Whitespace | Comment

  let regex_strings : (other_regexes * string) list =
    [ Id,          "[a-zA-Z][a-zA-Z0-9_]*"
    ; Flt,         "[0-9]+\\.[0-9]+"
    ; Int,         "[0-9]+"
    ; Char,        "'[^'\\\\]'"
    ; CharEscape,  "'\\\\[nrt'\\\\]'"
    ; Whitespace,  "[ \n\r\t]+"
    ; Comment,     "#.*"
    ]
  
  let full_regex_strings : (other_regexes * string) list =
    List.map (fun (t,s) -> t, "^" ^ s ^ "$") regex_strings
  
  let full_other_regexes : (other_regexes * Str.regexp) list =
    List.map (fun (t,s) -> t, Str.regexp s) full_regex_strings

  let all_regexes : Str.regexp list =
    List.map (fun x -> Str.regexp (snd x)) regex_strings @ List.map fst simple_regexes
  
  let string_int_to_token (s:string) (start:int) : token =
    begin match Int64.of_string_opt s with
      | Some i -> LInt i
      | None   -> raise @@ LexError { t = "illegal integer format"; start = start; length = String.length s }
    end
  
  let string_flt_to_token (s:string) (start:int) : token =
    begin match Stdlib.float_of_string_opt s with
      | Some f -> LFlt f
      | None   -> raise @@ LexError { t = "illegal floating point number format" ; start = start; length = String.length s }
    end
  
  let string_char_to_token (s:string) (start:int) : token =
    let s' = String.sub s 1 (String.length s - 2) in
    if String.length s' = 1 then LChar (String.get s' 0)
    else if String.length s' = 2 then
      LChar (begin match String.get s' 1 with
                     | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r' | '\\' -> '\\' | '\'' -> '\''
                     | _ -> raise @@ LexError { t = "illegal escape character format"; start = start; length = String.length s }
                   end)
    else raise @@ LexError { t = "illegal character format"; start = start; length = String.length s }
  
  let string_string_to_token (s:string) (start:int) : token =
    let rec treat_string (c : char list) : string =
      let escape_to_char : (char * char) list =
        [ 'n', '\n' ; 'r', '\r' ; 't', '\t' ; '\\', '\\' ; '\'', '\'' ; '"', '"' ]
      in
      begin match c with 
        | []          -> ""
        | '\''::c::cs ->
            begin match List.assoc_opt c escape_to_char with
              | Some c -> String.make 1 c
              | None   -> raise @@ LexError { t = "illegal escape character format"; start = start; length = String.length s }
            end ^ treat_string cs
        | c::cs       -> String.make 1 c ^ treat_string cs
      end
    in
    let s' = String.sub s 1 (String.length s - 2) in
    let charlist = List.init (String.length s') (String.get s') in
    LStr (treat_string charlist)

  let string_id_to_token (s:string) (start:int) : token =
    begin match List.assoc_opt s keywords with
      | None   -> Id s
      | Some t -> t
    end
  
  let as_token_node (s:string) (start:int) f : token node =
    { t = f s start ; start = start ; length = String.length s }
  
  let lex (file:string) : token node list =
    (* returns the matching substring of s starting at index i, if it exists *)
    let findmatch (s:string) (i:int) : string =
      let rec aux = function
        | []    -> raise @@ LexError { t = "Unmatched Character"; start = i; length = 1 }
        | r::rs -> if Str.string_match r s i then Str.matched_string s else aux rs
      in
      aux all_regexes
    in

    (* comments leave multiple whitespaces behind one another *)
    let rec remove_multiple_whitespaces (prog : token node list) =
      begin match prog with
        | []       -> []
        | [x]      -> [x]
        | x::y::xs -> 
            begin match x.t, y.t with
              | Whitespace _, Whitespace _ -> remove_multiple_whitespaces (y::xs)
              | _                          -> x :: remove_multiple_whitespaces (y::xs)
            end
      end
    in

    let remove_trailing_whitespace (prog : token node list) =
      let rec aux (prog : token node list) =
        begin match prog with
          | {t=Whitespace _;start=_;length=_}::ps -> aux ps
          | p -> p
        end
      in
      List.rev (aux (List.rev prog))
    in

    (* assumes prog[startindex-1] contains the starting '"' or the previous string part *)
    let rec lex_string (prog : string) (carry : string) (startindex : int) : string * int =
      if startindex >= String.length prog then raise @@ LexError { t = "Unterminated String"; start = startindex-1; length = 1 } else ();
      let nextchar, nextindex, term =
        begin match String.get prog startindex with
          | '\\' ->
              if startindex+1 >= String.length prog then raise @@ LexError { t = "Unterminated String"; start = startindex; length = 1 } else ();
              begin match String.get prog (startindex+1) with
                | 'n'  -> '\n'
                | 'r'  -> '\r'
                | 't'  -> '\t'
                | '\'' -> '\''
                | '\"' -> '\"'
                | '\\' -> '\\'
                | _    -> raise @@ LexError { t = "Unknown escape character"; start = startindex; length = 2 }
              end, startindex + 2, false
          | '\"' -> '\x00', startindex+1, true (* nextchar is omitted here *)
          | '\n' | '\t' | '\r' ->
              raise @@ LexError { t = "Illegal Character in String"; start = startindex; length = 1 }
          | c    -> c, startindex+1, false
        end in
      if term then
        carry, nextindex
      else
        lex_string prog (carry ^ String.make 1 nextchar) nextindex
    in

    let rec resolve_whitespaces (prog : token node list) : token node list =
      List.map
        Token.(fun t ->
          match t.t with
            | Whitespace (Right s) ->
                let sizes = String.fold_right (fun c l -> if c = '\t' then tabsize::l else 1::l) s [] in
                ofnode (Whitespace (Left (List.fold_left (+) 0 sizes))) t
            | _ -> t
        )
        prog
    in

    let rec aux (startindex : int) (prog : string) : token node list =
      if startindex >= String.length prog then
        []
      else if String.starts_with ~prefix:"\"" (String.sub prog startindex (String.length prog - startindex)) then
        let smatch, nextindex = lex_string prog "" (startindex+1) in
        { t = LStr smatch ; start = startindex ; length = nextindex - startindex } :: aux nextindex prog
      else
        let m = findmatch prog startindex in
        if Str.string_match (List.assoc Whitespace full_other_regexes) m 0 then
          (if String.contains m '\n' then
            let s = List.hd @@ List.rev @@ String.split_on_char '\n' m in
            [ { t = Token.Whitespace (Right s) ; start = startindex + String.length m - String.length s ; length = String.length s } ]
          else
            []) @
          aux (startindex + String.length m) prog
        else (
            if Str.string_match (List.assoc Id full_other_regexes) m 0 then
              [ as_token_node m startindex string_id_to_token ]
            else if Str.string_match (List.assoc Flt full_other_regexes) m 0 then
              [ as_token_node m startindex string_flt_to_token ]
            else if Str.string_match (List.assoc Int full_other_regexes) m 0 then
              [ as_token_node m startindex string_int_to_token ]
            else if Str.string_match (List.assoc Char full_other_regexes) m 0
                || Str.string_match (List.assoc CharEscape full_other_regexes) m 0 then
              [ as_token_node m startindex string_char_to_token ]
            else if Str.string_match (List.assoc Comment full_other_regexes) m 0 then
              []
            else
              begin match List.assoc_opt m simple_regex_strings with
                | None   -> raise @@ LexError { t = "unknown operator"; start = startindex; length = String.length m }
                | Some t -> [ { t = t ; start = startindex ; length = String.length m } ]
              end
          ) @ aux (startindex + String.length m) prog
    in

    let matches = aux 0 ("\n" ^ file) in
    let matches' = remove_multiple_whitespaces matches in
    let matches'' = remove_trailing_whitespace matches' in
    let matches''' = resolve_whitespaces matches'' in
    if List.length matches''' > 0 then
      matches''' @ [ { t = EOF ; start = (let last = (List.hd (List.rev matches''')) in last.start + last.length); length = 0 } ]
    else
      [ { t = EOF ; start = 0 ; length = 0 } ]

end