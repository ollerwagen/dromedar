open Common
open Ast
open Token
open Lexer

module Parser = struct

  type state = Token.token node list

  exception ParseError of string node

  let precedences : Token.op list list = Token.[
      [ Logor ]
    ; [ Logxor ]
    ; [ Logand ]
    ; [ Equal; NotEqual; Greater; Less; GreaterEq; LessEq; RefEqual; RefNotEqual ]
    ; [ Bitor ]
    ; [ Bitxor ]
    ; [ Bitand ]
    ; [ LShift; RShift; AShift ]
    ; [ Plus; Dash ]
    ; [ Star; Slash; Percent ]
    ; [ Starstar ]
    ]

  type prec = | LeftAssoc | RightAssoc

  let binary_assocs : (bop * prec) list = [
      Pow,       RightAssoc
    ; Mul,       LeftAssoc
    ; Div,       LeftAssoc
    ; Mod,       LeftAssoc
    ; Add,       LeftAssoc
    ; Sub,       LeftAssoc
    ; Shl,       LeftAssoc
    ; Shr,       LeftAssoc
    ; Sha,       LeftAssoc
    ; Bitand,    LeftAssoc
    ; Bitxor,    LeftAssoc
    ; Bitor,     LeftAssoc
    ; Logand,    LeftAssoc
    ; Logxor,    LeftAssoc
    ; Logor,     LeftAssoc
    ]
  
  let uops : Token.token list = Token.[ Op Bang ; Op Dash ]
  let cmpops : Token.token list = Token.[ Op Equal ; Op NotEqual ; Op Greater ; Op GreaterEq ; Op Less ; Op LessEq ; Op RefEqual ; Op RefNotEqual ]

  let uop_from_token : (Token.token * uop) list = [
      Token.Op Token.Bang, Not
    ; Token.Op Token.Dash, Neg
    ]
  
  let bop_from_token : (Token.token * bop) list = [
      Token.Op Token.Starstar, Pow
    ; Token.Op Token.Star,     Mul
    ; Token.Op Token.Slash,    Div
    ; Token.Op Token.Percent,  Mod
    ; Token.Op Token.Plus,     Add
    ; Token.Op Token.Dash,     Sub
    ; Token.Op Token.LShift,   Shl
    ; Token.Op Token.RShift,   Shr
    ; Token.Op Token.AShift,   Sha
    ; Token.Op Token.Bitand,   Bitand
    ; Token.Op Token.Bitxor,   Bitxor
    ; Token.Op Token.Bitor,    Bitor
    ; Token.Op Token.Logand,   Logand
    ; Token.Op Token.Logxor,   Logxor
    ; Token.Op Token.Logor,    Logor
    ]
  
  let cmpop_from_token : (Token.token * cmpop) list =
    [ Token.Op Token.Equal,       Eq
    ; Token.Op Token.NotEqual,    Neq
    ; Token.Op Token.Greater,     Greater
    ; Token.Op Token.Less,        Less
    ; Token.Op Token.GreaterEq,   GreaterEq
    ; Token.Op Token.LessEq,      LessEq
    ; Token.Op Token.RefEqual,    RefEq
    ; Token.Op Token.RefNotEqual, RefNotEq
    ]

  let peek : state -> Token.token node = List.hd

  let advance (s:state) (indent : int) : Token.token node * state = List.hd s, List.tl s

  let expect (s:state) (indent : int) (t:Token.token) : state =
    if (peek s).t = t then snd @@ advance s indent
    else raise (ParseError (ofnode (Printf.sprintf "Expected %s" (Token.print_token t)) (peek s)))
  
  let matches_indent_and (s:state) (indent:int) (t:Token.token) : bool =
    if (peek s).t = Token.Whitespace (Left indent) then
      (peek (snd (advance s indent))).t = t
    else false
  
  let rec parse_ty (s : state) (indent : int) : ty node * state =
    let start = (peek s).start in
    let t,s' =
      begin match (peek s).t with
        | Token.KInt   -> TInt,  snd @@ advance s indent
        | Token.KFlt   -> TFlt,  snd @@ advance s indent
        | Token.KChar  -> TChar, snd @@ advance s indent
        | Token.KBool  -> TBool, snd @@ advance s indent
        | Token.Op Token.Less -> let t,s' = parse_templty s indent in t.t, s'
        | Token.LParen -> let t,s' = parse_paren_ty s indent in t.t, s'
        | _           -> let rt,s' = parse_rty s indent in rt.t, s'
      end in
    { t = t ; start = start ; length = (peek s').start - start }, s'

  and parse_paren_ty (s : state) (indent : int) : ty node * state =
    let start = (peek s).start in
    let s' = expect s indent Token.LParen in
    let t,s'' = parse_ty s' indent in
    let tlist,s''' =
      begin match (peek s'').t with
        | Token.RParen -> [t], snd @@ advance s'' indent
        | Token.Comma  ->
            let ts,s''' = parse_ty_list s'' indent in
            let s'''' = expect s''' indent Token.RParen in
            ts, s''''
        | _ -> raise @@ ParseError (ofnode ("expected ')' or ',' after type list in parentheses") (peek s''))
      end in
    begin match (peek s''').t with
      | Token.Arrow -> 
          let s'''' = snd @@ advance s''' indent in
          let rt,s''''' = parse_retty s'''' indent in
          { t = TRef (TFun (List.map (fun n -> n.t) tlist, rt.t)) ; start = start ; length = (peek s''''').start - start }, s'''''
      | Token.QuestionMark ->
          begin match tlist with
            | [t] ->
                begin match t.t with
                  | TRef rt -> ofnode (TNullRef rt) t, snd @@ advance s''' indent
                  | _       -> raise @@ ParseError (ofnode "? can only succeed a non-null reference type" t)
                end
            | _ -> raise @@ ParseError (ofnode "cannot have maybe-null type of (...) type with multiple or no types in parentheses" (peek s'''))
          end
      | _ ->
          begin match tlist with
            | [t] -> t, s'''
            | _   -> raise @@ ParseError (ofnode "expected a function arrow after argument type list" (peek s'''))
          end
    end

  and parse_ty_list (s : state) (indent : int) : ty node list * state =
    begin match (peek s).t with
      | Token.Comma ->
          let s' = snd @@ advance s indent in
          let t,s'' = parse_ty s' indent in
          let ts,s''' = parse_ty_list s'' indent in
          t :: ts, s'''
      | _ -> [], s
    end

  and parse_templty (s : state) (indent : int) : ty node * state =
    let start = (peek s).start in
    let s' = expect s indent (Token.Op Token.Less) in
    let id,s'' =
      begin match (peek s').t with
        | Token.Id id -> id, snd @@ advance s' indent
        | _           -> raise @@ ParseError (ofnode ("expected an identifier in generic type") (peek s'))
      end in
    let s''' = expect s'' indent (Token.Op Token.Greater) in
    let notnull,s'''' =
      begin match (peek s''').t with
        | Token.QuestionMark -> false, snd @@ advance s''' indent
        | _                  -> true, s'''
      end in
    { t = TTempl(notnull,id) ; start = start ; length = (peek s'''').start - start }, s''''

  and parse_rty (s : state) (indent : int) : ty node * state =
    let parse_arr_ty (s : state) : rty * state =
      let s' = expect s indent Token.LBrack in
      let t,s'' = parse_ty s' indent in
      let s''' = expect s'' indent Token.RBrack in
      TArr t.t, s'''
    in
    let start = (peek s).start in
    let t,s' = 
      begin match (peek s).t with
        | Token.LBrack  -> parse_arr_ty s
        | Token.KString ->
            let _,s' = advance s indent in
            TStr, s'
        | Token.Id id ->
            let s' = snd @@ advance s indent in
            begin match (peek s').t with
              | Token.Dot ->
                  let s'' = snd @@ advance s' indent in
                  begin match (peek s'').t with
                    | Token.Id id' -> TModNamed (id,id'), snd @@ advance s'' indent
                    | _ -> raise @@ ParseError (ofnode "Expected a type name" (peek s''))
                  end
              | _ -> TNamed id, s'
            end
        | _             -> raise @@ ParseError (ofnode "Expected a reference type" (peek s))
      end in
    if (peek s').t = QuestionMark then
      let s'' = snd (advance s' indent) in
      { t = TNullRef t ; start = start ; length = (peek s'').start - start }, s''
    else
      { t = TRef t ; start = start ; length = (peek s').start - start }, s'

  and parse_retty (s : state) (indent : int) : retty node * state =
    let start = (peek s).start in
    begin match (peek s).t with
      | Token.KVoid -> 
          let _,s' = advance s indent in
          { t = Void ; start = start ; length = (peek s').start - start }, s'
      | _ ->
          let t,s' = parse_ty s indent in { t = Ret t.t ; start = start ; length = (peek s').start - start }, s'
    end

  let rec parse_exp (s : state) (indent : int) : exp node * state =

    let rec parse_commalist (s : state) (indent : int) : exp node list * state =
      if (peek s).t = Token.Comma then
        let s' = snd @@ advance s indent in
        let e, s'' = parse_exp s' indent in
        let rs,s''' = parse_commalist s'' indent in
        e::rs, s'''
      else
        [], s
    in

    let rec parse_binary_expression (prec_lvl : int) (s : state) (indent : int) : exp node * state =

      let rec parse_binary_partial (prec_lvl : int) (s : state) (indent : int) (lhs : exp node) : exp node * state =
        let oplist = List.nth precedences prec_lvl in
        if List.exists (fun op -> Token.Op op = (peek s).t) oplist then
          let op,s' = advance s indent in
          let op_as_bop = List.assoc op.t bop_from_token in
          let rhs,s'' = parse_binary_expression (prec_lvl+1) s' indent in
          let res = { t = Bop (op_as_bop, lhs, rhs) ; start = lhs.start ; length = rhs.start - lhs.start + rhs.length } in
          parse_binary_partial prec_lvl s'' indent res
        else
          lhs, s
      in

      let rec parse_listop_partial (prec_lvl : int) (s : state) (indent : int) (first : exp node) (startlist : (cmpop * exp node) list) : exp node * state =
        if List.exists (fun t -> t = (peek s).t) cmpops then
          let op,s' = advance s indent in
          let op_as_cmpop = List.assoc op.t cmpop_from_token in
          let next,s'' = parse_binary_expression (prec_lvl+1) s' indent in
          let cmplist = startlist @ [ op_as_cmpop, next ] in
          parse_listop_partial prec_lvl s'' indent first cmplist
        else
          let last_elem = List.hd @@ List.rev startlist in
          { t = Cmps (first, startlist) ; start = first.start ; length = (snd last_elem).start - first.start + (snd last_elem).start }, s
      in

      if prec_lvl >= List.length precedences then
        parse_unary_expression s indent
      else
        let start = (peek s).start in
        let lhs,s' = parse_binary_expression (prec_lvl+1) s indent in
        let oplist = List.nth precedences prec_lvl in
        if List.exists (fun op -> Token.Op op = (peek s').t) oplist then
          let op, s'' = advance s' indent in
          if List.mem op.t cmpops then
            parse_listop_partial (prec_lvl+1) s' indent lhs []
          else
            let op_as_bop = List.assoc op.t bop_from_token in
            begin match List.assoc op_as_bop binary_assocs with
              | LeftAssoc ->
                  let rhs,s''' = parse_binary_expression (prec_lvl+1) s'' indent in
                  let resnode = { t = Bop (op_as_bop, lhs, rhs) ; start = start ; length = rhs.start - start + rhs.length } in
                  parse_binary_partial prec_lvl s''' indent resnode
              | RightAssoc ->
                  let rhs,s''' = parse_binary_expression prec_lvl s'' indent in
                  { t = Bop (op_as_bop, lhs, rhs) ; start = start ; length = rhs.start - start + rhs.length }, s'''
            end
        else
          lhs, s'

    and parse_unary_expression (s : state) (indent : int) : exp node * state =
      if List.mem (peek s).t uops then
        let start = (peek s).start in
        let op, s' = advance s indent in
        let e, s'' = parse_unary_expression s' indent in
        { t = Uop ((List.assoc op.t uop_from_token), e) ; start = start ; length = e.start - start + e.length }, s''
      else
        parse_simple_expression s indent
    
    and parse_sprintf_expression (s : state) (indent : int) (start_t : token) : exp node * state =
      let start = (peek s).start in
      let s' = expect s indent start_t in
      let s'' = expect s' indent Token.LParen in
      let str,s''' =
        begin match (peek s'').t with
          | LStr s -> let snode,s''' = advance s'' indent in ofnode s snode, s'''
          | _ -> raise @@ ParseError (ofnode "Expect a string literal in sprintf expression" (fst @@ advance s'' indent))
        end in
      let es,s'''' = parse_commalist s''' indent in
      let s''''' = expect s'''' indent Token.RParen in
      { t = Sprintf ((if start_t = Token.KSprintf then Sprintf else Printf), str, es) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_simple_expression (s : state) (indent : int) : exp node * state =
      let start = (peek s).start in
      let lhs,s' = parse_basic_expression s indent in
      parse_optional_application start lhs s' indent
    
    and parse_optional_application (start : int) (lhs : exp node) (s : state) (indent : int) : exp node * state =
      begin match (peek s).t with
        | Token.LParen ->
            let e,s' = parse_application_expression start lhs s indent in
            parse_optional_application start e s' indent
        | Token.LBrack -> 
            let e,s' = parse_subscript_expression start lhs s indent in
            parse_optional_application start e s' indent
        | Token.Dot ->
            let e,s' = parse_projection_expression start lhs s indent in
            parse_optional_application start e s' indent
        | _            -> lhs, s
      end
    
    and parse_commasep_explist (s : state) (indent : int) (delim : token) (allow_empty : bool) : exp option node list * state =
      if (peek s).t = delim && allow_empty then
        [], s
      else
        let e,s' =
          begin match (peek s).t with
            | Token.Underscore -> ofnode None (peek s), snd @@ advance s indent
            | _                -> let e,s' = parse_exp s indent in ofnode (Some e.t) e, s'
          end in
        if (peek s').t = Comma then
          let s'' = snd (advance s' indent) in
          let rest, s''' = parse_commasep_explist s'' indent delim false in
          e :: rest, s'''
        else
          [e], s'
    
    and parse_basic_expression (s : state) (indent : int) : exp node * state =

      let rec parse_listcompvars (s : state) (indent : int) : (string * exp node) list * state =
        let id,s' =
          begin match (peek s).t with
            | Id id -> id, snd @@ advance s indent
            | _ -> raise @@ ParseError (ofnode "expected an identifier in list comprehension variable list" (peek s))
          end in
        let s'' = expect s' indent KIn in
        let e,s''' = parse_exp s'' indent in
        begin match (peek s''').t with
          | Comma -> let vs,s'''' = parse_listcompvars (snd @@ advance s''' indent) indent in (id,e)::vs, s''''
          | _     -> [id,e], s'''
        end
      in

      let start = (peek s).start in
      begin match (peek s).t with
        | LParen -> (* grouping expression *)
            let _,s' = advance s indent in
            let e,s'' = parse_exp s' indent in
            let s''' = expect s'' indent RParen in
            e, s'''
        | LBrack -> (* array literal *)
            let _,s' = advance s indent in
            let e,s'' = parse_exp s' indent in
            begin match (peek s'').t with
              | Token.Comma | Token.RBrack ->
                  let es,s''' = parse_commalist s'' indent in
                  let s'''' = expect s''' indent RBrack in
                  { t = LitArr (e::es) ; start = start ; length = (peek s'''').start - start }, s''''
              | Token.Colon ->
                  let s''' = snd @@ advance s'' indent in
                  let vs,s'''' = parse_listcompvars s''' indent in
                  let c,s''''' =
                    begin match (peek s'''').t with
                      | Colon -> 
                          let s''''' = snd @@ advance s'''' indent in
                          parse_exp s''''' indent
                      | _ ->
                          ofnode (LitBool true) (peek s''''), s''''
                    end in
                  let s'''''' = expect s''''' indent RBrack in
                  { t = ListComp (e,vs,c) ; start = start ; length = (peek s'''''').start - start }, s''''''
              | _ ->
                  let incl,excl =
                    begin match (peek s'').t with
                      | Token.Dots        -> Incl, Incl
                      | Token.DotsPipe    -> Incl, Excl
                      | Token.PipeDots    -> Excl, Incl
                      | Token.PipeDotPipe -> Excl, Excl
                      | _ -> raise @@ ParseError (ofnode "Expected a range specifier in array expression" (peek s))
                    end in
                  let ee,s''' = parse_exp (snd @@ advance s'' indent) indent in
                  let s'''' = expect s''' indent RBrack in
                  { t = RangeList (e,incl,excl,ee) ; start = start ; length = (peek s''').start - start }, s''''
            end
            
        | KPrintf  -> parse_sprintf_expression s indent KPrintf
        | KSprintf -> parse_sprintf_expression s indent KSprintf
        | LInt  i -> let _,s' = advance s indent in { t = LitInt  i ; start = start ; length = (peek s').start - start }, s'
        | LFlt  f -> let _,s' = advance s indent in { t = LitFlt  f ; start = start ; length = (peek s').start - start }, s'
        | LChar c -> let _,s' = advance s indent in { t = LitChar c ; start = start ; length = (peek s').start - start }, s'
        | LBool b -> let _,s' = advance s indent in { t = LitBool b ; start = start ; length = (peek s').start - start }, s'
        | LStr  c -> let _,s' = advance s indent in { t = LitStr  c ; start = start ; length = (peek s').start - start }, s'
        | Id    i -> let _,s' = advance s indent in { t = Id      i ; start = start ; length = (peek s').start - start }, s'
        | _             -> raise @@ ParseError (ofnode "Expected a simple expression (literal or identifier)" (peek s))
      end
    
    and parse_subscript_expression (start : int) (lhs : exp node) (s : state) (indent : int) : exp node * state =
      let t,s' = advance s indent in
      begin match t.t with
        | Token.LBrack ->
            let rhs,s'' = parse_exp s' indent in
            let s''' = expect s'' indent Token.RBrack in
            { t = Subscript (lhs,rhs) ; start = start ; length = (peek s''').start - start }, s'''
        | _ -> raise @@ ParseError (ofnode "Malformed Subscript expression" t)
      end

    and parse_application_expression (start : int) (lhs : exp node) (s : state) (indent : int) : exp node * state =
      let t,s' = advance s indent in
      begin match t.t with
        | Token.LParen ->
            let args,s'' = parse_commasep_explist s' indent RParen true in
            let s''' = expect s'' indent Token.RParen in
            let t = { t = FApp (lhs, args) ; start = start ; length = (peek s''').start - start } in
            begin match (peek s''').t with
              | Token.LParen -> parse_application_expression start t s''' indent
              | _            -> t, s'''
            end
        | _ -> raise @@ ParseError (ofnode "Malformed application expression" t)
      end

    and parse_projection_expression (start : int) (lhs : exp node) (s : state) (indent : int) : exp node * state =
      let t,s' = advance s indent in
      begin match t.t with
        | Token.Dot ->
            begin match (peek s').t with
              | Token.Id id ->
                  let snode,s'' = advance s' indent in
                  { t = Proj (lhs, ofnode id snode) ; start = start ; length = (peek s'').start }, s''
              | _ -> raise @@ ParseError (ofnode "expect an identifier in projection expression" (peek s'))
            end
        | _ -> raise @@ ParseError (ofnode "Malformed projection expression" t)
      end

    and parse_ternary_expression (s : state) (indent : int) : exp node * state =
      let start = (peek s).start in
      let s' = expect s indent QuestionMark in
      let c,s'' = parse_exp s' indent in
      let s''' = expect s'' indent Arrow in
      let e1,s'''' = parse_exp s''' indent in
      let s''''' = expect s'''' indent Colon in
      let e2,s'''''' = parse_exp s''''' indent in
      { t = Ternary (c,e1,e2) ; start = start ; length = (peek s'''''').start - start }, s''''''

    and parse_deref_expression (s : state) (indent : int) : exp node * state =
      let start = (peek s).start in
      let s' = expect s indent KAssert in
      let e,s'' = parse_exp s' indent in
      { t = Deref e ; start = start ; length = (peek s'').start - start }, s''
    in

    let start = (peek s).start in
    begin match (peek s).t with
      | KNull ->
          let s' = snd (advance s indent) in
          let t,s'' =
            begin match (peek s').t with
              | KOf ->
                  let t,s'' = parse_ty (snd @@ advance s' indent) indent in
                  Some t, s''
              | _   -> None, s'
            end in
          { t = Null t ; start = start ; length = (peek s'').start - start }, s''
      | LBrack -> (* either empty list or array literal *)
          let s' = snd (advance s indent) in
          begin match (peek s').t with
            | RBrack -> (* empty list *)
                let s'' = snd (advance s' indent) in
                begin match (peek s'').t with
                  | KOf ->
                      let t,s''' = parse_ty (snd @@ advance s'' indent) indent in
                      { t = EmptyList (Some t) ; start = start ; length = (peek s''').start - start }, s'''
                  | _ -> { t = EmptyList None ; start = start ; length = (peek s'').start - start }, s'' 
                end
            | _ -> (* arbitrary expression starting with '[' *)
                parse_binary_expression 0 s indent
          end
      | QuestionMark -> parse_ternary_expression s indent
      | KAssert -> parse_deref_expression s indent
      | _    -> parse_binary_expression 0 s indent
    end


  let rec parse_stmt (s : state) (indent : int) : stmt node * state =

    let rec parse_vdecl_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s',m =
        begin match (peek s).t with
          | KLet -> snd (advance s indent), Const
          | KMut -> snd (advance s indent), Mut
          | _    -> raise @@ ParseError (ofnode "Expected a declaration" (fst (advance s indent)))
        end in
      let id,s'' =
        begin match (peek s').t with
          | Id i -> i, snd @@ advance s' indent
          | _    -> raise @@ ParseError (ofnode "Declaration requires identifier" (fst @@ advance s' indent))
        end in
      let t,s''' =
        if (peek s'').t = Colon then
          let t,s''' = parse_ty (snd (advance s'' indent)) indent in Some t, s'''
        else None, s'' in
      let s'''' = expect s''' indent Token.Assign in
      let e,s''''' = parse_exp s'''' indent in
      { t = VDecl (id, m, t, e) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_assert_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent KAssert in
      let e,s'' = parse_exp s' indent in
      { t = Assert e ; start = start ; length = (peek s'').start - start }, s''

    and parse_expr_or_assn_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let l,s' = parse_exp s indent in
      if (peek s').t = Token.Assign then
        let _,s'' = advance s' indent in
        let r,s''' = parse_exp s'' indent in
        { t = Assn (l,r) ; start = start ; length = (peek s''').start - start }, s'''
      else
        { t = Expr l ; start = start ; length = (peek s').start - start }, s'

    and parse_if_stmt (s : state) (indent : int) (ifkey : Token.token) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent ifkey in
      let e,s'' = parse_exp s' indent in
      let b,s''' = parse_block s'' (indent+1) in
      if matches_indent_and s''' indent Token.KElif then
        let _,s'''' = advance s''' indent in
        let es,s''''' = parse_if_stmt s'''' indent (Token.KElif) in
        { t = If (e, b, [es]) ; start = start ; length = (peek s''''').start - start }, s'''''
      else if matches_indent_and s''' indent Token.KElse then
        let _,s'''' = advance (snd (advance s''' indent)) indent in
        let n,s''''' = parse_block s'''' (indent+1) in
        { t = If (e, b, n) ; start = start ; length = (peek s''''').start - start }, s'''''
      else
        { t = If (e, b, []) ; start = start ; length = (peek s''').start - start }, s'''

    and parse_denull_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KDenull in
      let id,s'' =
        begin match (peek s').t with
          | Id id -> id, snd @@ advance s' indent
          | _     -> raise @@ ParseError (ofnode "Expected an identifier" (fst @@ advance s' indent))
        end in
      let s''' = expect s'' indent Token.Assign in
      let e,s'''' = parse_exp s''' indent in
      let b,s''''' = parse_block s'''' (indent+1) in
      if matches_indent_and s''''' indent Token.KElse then
        let s'''''' = snd @@ advance (snd @@ advance s''''' indent) indent in
        let b',s''''''' = parse_block s'''''' (indent+1) in
        { t = Denull (id, e, b, b') ; start = start ; length = (peek s''''''').start - start }, s'''''''
      else
        { t = Denull (id, e, b, []) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_while_stmt (s : state) (indent : int) : stmt node * state = 
      let start = (peek s).start in
      let s' = expect s indent Token.KWhile in
      let c,s'' = parse_exp s' indent in
      let b,s''' = parse_block s'' (indent+1) in
      { t = While (c,b) ; start = start ; length = (peek s''').start - start }, s'''

    and parse_dowhile_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KDo in
      let b,s'' = parse_block s' (indent + 1) in
      let s''' = expect s'' indent (Token.Whitespace (Left indent)) in
      let s'''' = expect s''' indent Token.KWhile in
      let c,s''''' = parse_exp s'''' indent in
      { t = DoWhile (c,b) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_for_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KFor in
      let id,s'' =
        begin match (peek s').t with
          | Token.Id i -> i, snd @@ advance s' indent
          | _          -> raise @@ ParseError (ofnode "For loop requires loop variable" (fst @@ advance s' indent))
        end in
      begin match (peek s'').t with
        | Token.Assign ->
            let s''' = snd @@ advance s'' indent in
            let startexp,s'''' = parse_exp s''' indent in
            let incl1,incl2,s''''' =
              begin match (peek s'''').t with
                | Token.Dots        -> Incl, Incl, snd @@ advance s'''' indent
                | Token.DotsPipe    -> Incl, Excl, snd @@ advance s'''' indent
                | Token.PipeDots    -> Excl, Incl, snd @@ advance s'''' indent
                | Token.PipeDotPipe -> Excl, Excl, snd @@ advance s'''' indent
                | _              -> raise @@ ParseError (ofnode "expect a range indicator in for header" (fst @@ advance s'''' indent))
              end in
            let endexp,s'''''' = parse_exp s''''' indent in
            let b,s''''''' = parse_block s'''''' (indent+1) in
            { t = For (id,startexp,incl1,incl2,endexp,b) ; start = start ; length = (peek s''''''').start - start }, s'''''''
        | Token.KIn ->
            let s''' = snd @@ advance s'' indent in
            let lexp,s'''' = parse_exp s''' indent in
            let b,s''''' = parse_block s'''' (indent+1) in
            { t = ForIn (id,lexp,b) ; start = start ; length = (peek s''''').start - start }, s'''''
        | _ -> raise @@ ParseError (ofnode "expected either an assignment or a range list with 'in'" (peek s''))
      end

    and parse_break_stmt (s : state) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KBreak in
      { t = Break ; start = start ; length = (peek s').start - start }, s'

    and parse_continue_stmt (s : state) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KContinue in
      { t = Continue ; start = start ; length = (peek s').start - start }, s'

    and parse_return_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KReturn in
      let t,_ = advance s' indent in
      begin match t.t with
        | Token.EOF | Token.Whitespace _ ->
            { t = Return None ; start = start ; length = (peek s').start - start }, s'
        | _ ->
            let e,s'' = parse_exp s' indent in
            { t = Return (Some e) ; start = start ; length = (peek s'').start - start }, s''
      end
    in

    let s' = expect s indent (Token.Whitespace (Left indent)) in
    begin match (peek s').t with
      | Token.KLet | Token.KMut -> parse_vdecl_stmt        s' indent
      | Token.KAssert           -> parse_assert_stmt       s' indent
      | Token.KIf               -> parse_if_stmt           s' indent Token.KIf
      | Token.KDenull           -> parse_denull_stmt       s' indent
      | Token.KWhile            -> parse_while_stmt        s' indent
      | Token.KDo               -> parse_dowhile_stmt      s' indent
      | Token.KFor              -> parse_for_stmt          s' indent
      | Token.KBreak            -> parse_break_stmt        s'
      | Token.KContinue         -> parse_continue_stmt     s'
      | Token.KReturn           -> parse_return_stmt       s' indent
      | _                       -> parse_expr_or_assn_stmt s' indent
    end

  and parse_block (s : state) (indent : int) : stmt node list * state =
    let rec aux (s : state) (indent : int) (fixed : bool) : stmt node list * state =
      begin match (peek s).t with
        | Whitespace (Left ind') ->
            if fixed && ind' = indent || not fixed && ind' >= indent then
              let st,s' = parse_stmt s ind' in
              let b,s'' = aux s' ind' true in
              st :: b, s''
            else
              [], s
        | _ -> [], s
      end
    in
    aux s indent false


  let parse_gstmt (indent:int) (s:state) : gstmt node * state =

    let rec parse_gvdecl (s:state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KGlobal in
      let s'',m =
        if (peek s').t = KMut then snd (advance s' indent), Mut
        else s', Const in
      let id,s''' =
        begin match (peek s'').t with
          | Id id -> id, snd (advance s'' indent)
          | _     -> raise @@ ParseError (ofnode "Declaration requires identifier" (fst (advance s'' indent)))
        end in
      let t,s'''' =
        if (peek s''').t = Colon then
          let t,s'''' = parse_ty (snd (advance s''' indent)) indent in Some t, s''''
        else
          None, s''' in
      let s''''' = expect s'''' indent Token.Assign in
      let e,s'''''' = parse_exp s''''' indent in
      { t = GVDecl (id, m, t, e) ; start = start ; length = (peek s'''''').start - start }, s''''''

    and parse_gfdecl (s:state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KFn in
      let id,s'' =
        begin match advance s' indent with
          | {t=Token.Id i;start=_;length=_},s'' -> i,s''
          | t,s''                               -> raise @@ ParseError (ofnode "Function declaration requires identifier" t)
        end in
      let arglist, s''' =
        begin match advance s'' indent with
          | {t=Token.Arrow;start=_;length=_}, s''' -> [], s''
          | {t=Token.LParen;start=_;length=_}, s''' ->
              let rec args (s:state) : (string * ty node) list * state =
                let id,s' =
                  begin match advance s indent with
                    | {t=Token.Id i;start=_;length=_},s' -> i,s'
                    | _                                  -> raise @@ ParseError (ofnode "Expected a function argument name" (peek s))
                  end in
                let s'' = expect s' indent Token.Colon in
                let t,s''' = parse_ty s'' indent in
                if (peek s''').t = Token.Comma then
                  let _,s'''' = advance s''' indent in
                  let l,s''''' = args s'''' in
                  (id, t) :: l, s'''''
                else
                  [ id, t ], s'''
              in
              let arglist, s'''' = args s''' in
              let s''''' = expect s'''' indent Token.RParen in
              arglist, s'''''
          | _ -> raise @@ ParseError (ofnode "Expected a separator in function argument list" (peek s))
        end in
      let s'''' = expect s''' indent Token.Arrow in
      let rt,s''''' = parse_retty s'''' indent in
      let b,s'''''' = parse_block s''''' 1 in
      { t = GFDecl (id,arglist,rt,b) ; start = start ; length = (peek s'''''').start - start }, s''''''
    
    and parse_gmodule (s : state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KModule in
      begin match (peek s').t with
        | Token.Id m ->
            let s'' = snd @@ advance s' indent in
            { t = Module m ; start = start ; length = (peek s'').start - start }, s''
        | _ -> raise @@ ParseError (ofnode "Expected an identifier in module declaration" (peek s'))
      end

    and parse_gnative (s : state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s indent Token.KNative in
      begin match (peek s').t with
        | Token.KFn   -> parse_gnfdecl start s'
        | Token.KType -> parse_gntdecl start s'
        | Token.Id _  -> parse_gnvdecl start s'
        | _           -> raise @@ ParseError (ofnode "Expected a native declaration" (peek s'))
      end

    and parse_gnfdecl (start : int) (s : state) : gstmt node * state =
      let s' = expect s indent Token.KFn in
      let id,s'' =
        begin match (peek s').t with
          | Token.Id id -> id, snd @@ advance s' indent
          | _           -> raise @@ ParseError (ofnode "Expected a native function name" (peek s'))
        end in
      let arglist, s''' =
        begin match advance s'' indent with
          | {t=Token.Arrow;start=_;length=_}, s''' -> [], s''
          | {t=Token.LParen;start=_;length=_}, s''' ->
              let rec args (s:state) : ty node list * state =
                let t,s' = parse_ty s indent in
                if (peek s').t = Token.Comma then
                  let s'' = snd @@ advance s' indent in
                  let l,s''' = args s'' in
                  t :: l, s'''
                else
                  [ t ], s'
              in
              let arglist, s'''' = args s''' in
              let s''''' = expect s'''' indent Token.RParen in
              arglist, s'''''
          | _ -> raise @@ ParseError (ofnode "Expected a function argument list or ->" (peek s))
        end in
      let s'''' = expect s''' indent Token.Arrow in
      let rt,s''''' = parse_retty s'''' indent in
      { t = GNFDecl (id,arglist,rt) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_gnvdecl (start : int) (s : state) : gstmt node * state =
      let id,s' =
        begin match (peek s).t with
          | Id id -> id, snd @@ advance s indent
          | _     -> raise @@ ParseError (ofnode "Expected a native variable identifier" (peek s))
        end in
      let s'' = expect s' indent Token.Colon in
      let t,s''' = parse_ty s'' indent in
      { t = GNVDecl (id,t) ; start = start ; length = (peek s''').start - start }, s'''
    
    and parse_gntdecl (start : int) (s : state) : gstmt node * state =
      let s' = expect s indent Token.KType in
      let id,s'' =
        begin match (peek s').t with
          | Id id -> id, snd @@ advance s' indent
          | _     -> raise @@ ParseError (ofnode "Expected a native type identifier" (peek s'))
        end in
      { t = GNTDecl id ; start = start ; length = (peek s'').start - start }, s''

    in

    let s' = expect s indent (Token.Whitespace (Left indent)) in
    begin match (peek s').t with
      | Token.KModule -> parse_gmodule s'
      | Token.KGlobal -> parse_gvdecl s'
      | Token.KFn     -> parse_gfdecl s'
      | Token.KNative -> parse_gnative s'
      | _             -> raise @@ ParseError (ofnode "Expected a global variable or function declaration" (peek s'))
    end
  
  let parse (s : state) : gstmt node list =
    let rec aux (baseindent:int) (s:state) : gstmt node list =
      begin match s with
        | [] | [{t=Token.EOF;start=_;length=_}]             -> []
        | {t=Token.Whitespace (Left x);start=_;length=_}::_ ->
            if x = baseindent then
              let gs,s' = parse_gstmt baseindent s in gs :: aux baseindent s'
            else
              raise @@ ParseError (ofnode "Indent level in global scope must match among all global instructions" (peek s))
        | _ -> raise @@ ParseError (ofnode "expected whitespace resp. newline" (peek s))
      end
    in
    begin match s with
      | [] | [{t=Token.EOF}] -> []
      | {t=Token.Whitespace (Left x)}::_ -> aux x s
      | _ -> Stdlib.failwith "successful lex should start with a whitespace token"
    end

end