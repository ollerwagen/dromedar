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
    ; [ Star ]
    ; [ Starstar ]
    ]

  type prec = | LeftAssoc | RightAssoc

  let binary_assocs : (bop * prec) list = [
      Pow,       RightAssoc
    ; Mul,       LeftAssoc
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

  let advance (s:state) : Token.token node * state = List.hd s, List.tl s

  let expect (s:state) (t:Token.token) : state =
    if (peek s).t = t then snd @@ advance s
    else raise (ParseError (ofnode (Printf.sprintf "Expected %s" (Token.print_token t)) (peek s)))
  
  let matches_indent_and (s:state) (ind:int) (t:Token.token) : bool =
    if (peek s).t = Token.Whitespace ind then
      (peek (snd (advance s))).t = t
    else false
  
  let rec parse_ty (s : state) : ty node * state =
    let start = (peek s).start in
    let t,s' =
      begin match (peek s).t with
        | Token.KInt    -> TInt,  snd @@ advance s
        | Token.KFlt    -> TFlt,  snd @@ advance s
        | Token.KChar   -> TChar, snd @@ advance s
        | Token.KBool   -> TBool, snd @@ advance s
        | _             -> let rt,s' = parse_rty s in rt.t, s'
      end in
    { t = t ; start = start ; length = (peek s').start - start }, s'

  and parse_rty (s : state) : ty node * state =
    let parse_arr_ty (s : state) : rty * state =
      let s' = expect s Token.LBrack in
      let t,s'' = parse_ty s' in
      let s''' = expect s'' Token.RBrack in
      TArr t.t, s'''
    in
    let start = (peek s).start in
    let t,s' = 
      begin match (peek s).t with
        | Token.LBrack  -> parse_arr_ty s
        | Token.KString ->
            let _,s' = advance s in
            TStr, s'
        | _             -> raise @@ ParseError (ofnode "Expected a reference type" (peek s))
      end in
    if (peek s').t = QuestionMark then
      let s'' = snd (advance s') in
      { t = TNullRef t ; start = start ; length = (peek s'').start - start }, s''
    else
      { t = TRef t ; start = start ; length = (peek s').start - start }, s'

  and parse_retty (s : state) : retty node * state =
    let start = (peek s).start in
    begin match (peek s).t with
      | Token.KVoid -> 
          let _,s' = advance s in
          { t = Void ; start = start ; length = (peek s').start - start }, s'
      | _ ->
          let t,s' = parse_ty s in { t = Ret t.t ; start = start ; length = (peek s').start - start }, s'
    end

  let rec parse_exp (s : state) : exp node * state =

    let rec parse_binary_expression (prec_lvl : int) (s : state) : exp node * state =

      let rec parse_binary_partial (prec_lvl : int) (s : state) (lhs : exp node) : exp node * state =
        let oplist = List.nth precedences prec_lvl in
        if List.exists (fun op -> Token.Op op = (peek s).t) oplist then
          let op,s' = advance s in
          let op_as_bop = List.assoc op.t bop_from_token in
          let rhs,s'' = parse_binary_expression (prec_lvl+1) s' in
          let res = { t = Bop (op_as_bop, lhs, rhs) ; start = lhs.start ; length = rhs.start - lhs.start + rhs.length } in
          parse_binary_partial prec_lvl s'' res
        else
          lhs, s
      in

      let rec parse_listop_partial (prec_lvl : int) (s : state) (first : exp node) (startlist : (cmpop * exp node) list) : exp node * state =
        if List.exists (fun t -> t = (peek s).t) cmpops then
          let op,s' = advance s in
          let op_as_cmpop = List.assoc op.t cmpop_from_token in
          let next,s'' = parse_binary_expression (prec_lvl+1) s' in
          let cmplist = startlist @ [ op_as_cmpop, next ] in
          parse_listop_partial prec_lvl s'' first cmplist
        else
          let last_elem = List.hd @@ List.rev startlist in
          { t = Cmps (first, startlist) ; start = first.start ; length = (snd last_elem).start - first.start + (snd last_elem).start }, s
      in

      if prec_lvl >= List.length precedences then
        parse_unary_expression s
      else
        let start = (peek s).start in
        let lhs,s' = parse_binary_expression (prec_lvl+1) s in
        let oplist = List.nth precedences prec_lvl in
        if List.exists (fun op -> Token.Op op = (peek s').t) oplist then
          let op, s'' = advance s' in
          if List.mem op.t cmpops then
            parse_listop_partial (prec_lvl+1) s' lhs []
          else
            let op_as_bop = List.assoc op.t bop_from_token in
            begin match List.assoc op_as_bop binary_assocs with
              | LeftAssoc ->
                  let rhs,s''' = parse_binary_expression (prec_lvl+1) s'' in
                  let resnode = { t = Bop (op_as_bop, lhs, rhs) ; start = start ; length = rhs.start - start + rhs.length } in
                  parse_binary_partial prec_lvl s''' resnode
              | RightAssoc ->
                  let rhs,s''' = parse_binary_expression prec_lvl s'' in
                  { t = Bop (op_as_bop, lhs, rhs) ; start = start ; length = rhs.start - start + rhs.length }, s'''
            end
        else
          lhs, s'

    and parse_unary_expression (s : state) : exp node * state =
      if List.mem (peek s).t uops then
        let start = (peek s).start in
        let op, s' = advance s in
        let e, s'' = parse_unary_expression s' in
        { t = Uop ((List.assoc op.t uop_from_token), e) ; start = start ; length = e.start - start + e.length }, s''
      else
        parse_simple_expression s
    
    and parse_sprintf_expression (s : state) : exp node * state =
      let rec parse_commalist (s : state) : exp node list * state =
        if (peek s).t = Token.Comma then
          let s' = snd @@ advance s in
          let e, s'' = parse_exp s' in
          let rs,s''' = parse_commalist s'' in
          e::rs, s'''
        else
          [], s
      in
      let start = (peek s).start in
      let s' = expect s Token.KSprintf in
      let s'' = expect s' Token.LParen in
      let str,s''' =
        begin match (peek s'').t with
          | LStr s -> let snode,s''' = advance s'' in ofnode s snode, s'''
          | _ -> raise @@ ParseError (ofnode "Expect a string literal in sprintf expression" (fst @@ advance s''))
        end in
      let es,s'''' = parse_commalist s''' in
      let s''''' = expect s'''' Token.RParen in
      { t = Sprintf (str, es) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_simple_expression (s : state) : exp node * state =
      let start = (peek s).start in
      let lhs,s' = parse_basic_expression s in
      parse_optional_application start lhs s'
    
    and parse_optional_application (start : int) (lhs : exp node) (s : state) : exp node * state =
      begin match (peek s).t with
        | Token.LParen ->
            let e,s' = parse_application_expression start lhs s in
            parse_optional_application start e s'
        | Token.LBrack -> 
            let e,s' = parse_subscript_expression start lhs s in
            parse_optional_application start e s'
        | _            -> lhs, s
      end
    
    and parse_commasep_explist (s : state) (delim : token) (allow_empty : bool) : exp node list * state =
      if (peek s).t = delim && allow_empty then
        [], s
      else
        let e,s' = parse_exp s in
        if (peek s').t = Comma then
          let s'' = snd (advance s') in
          let rest, s''' = parse_commasep_explist s'' delim false in
          e :: rest, s'''
        else
          [e], s'
    
    and parse_basic_expression (s : state) : exp node * state =

      let start = (peek s).start in
      begin match (peek s).t with
        | LParen -> (* grouping expression *)
            let _,s' = advance s in
            let e,s'' = parse_exp s' in
            let s''' = expect s'' RParen in
            e, s'''
        | LBrack -> (* array literal *)
            let _,s' = advance s in
            let exps,s'' = parse_commasep_explist s' RBrack false in
            let s''' = expect s'' RBrack in
            { t = LitArr exps ; start = start ; length = (peek s''').start - start }, s'''
        | KSprintf -> parse_sprintf_expression s
        | LInt  i -> let _,s' = advance s in { t = LitInt  i ; start = start ; length = (peek s').start - start }, s'
        | LFlt  f -> let _,s' = advance s in { t = LitFlt  f ; start = start ; length = (peek s').start - start }, s'
        | LChar c -> let _,s' = advance s in { t = LitChar c ; start = start ; length = (peek s').start - start }, s'
        | LBool b -> let _,s' = advance s in { t = LitBool b ; start = start ; length = (peek s').start - start }, s'
        | LStr  c -> let _,s' = advance s in { t = LitStr  c ; start = start ; length = (peek s').start - start }, s'
        | Id    i -> let _,s' = advance s in { t = Id      i ; start = start ; length = (peek s').start - start }, s'
        | _             -> raise @@ ParseError (ofnode "Expected a simple expression (literal or identifier)" (peek s))
      end
    
    and parse_subscript_expression (start : int) (lhs : exp node) (s : state) : exp node * state =
      let t,s' = advance s in
      begin match t.t with
        | Token.LBrack ->
            let rhs,s'' = parse_exp s' in
            let s''' = expect s'' Token.RBrack in
            { t = Subscript (lhs,rhs) ; start = start ; length = (peek s''').start - start }, s'''
        | _ -> raise @@ ParseError (ofnode "Malformed Subscript expression" t)
      end

    and parse_application_expression (start : int) (lhs : exp node) (s : state) : exp node * state =
      let t,s' = advance s in
      begin match t.t with
        | Token.LParen ->
            let args,s'' = parse_commasep_explist s' RParen true in
            let s''' = expect s'' Token.RParen in
            let t = { t = FApp (lhs, args) ; start = start ; length = (peek s''').start - start } in
            begin match (peek s''').t with
              | Token.LParen -> parse_application_expression start t s''' 
              | _            -> t, s'''
            end
        | _ -> raise @@ ParseError (ofnode "Malformed application expression" t)
      end
    in

    let start = (peek s).start in
    begin match (peek s).t with
      | KNull ->
          let s' = snd (advance s) in
          let s'' = expect s' KOf in
          let t,s''' = parse_rty s'' in
          let t' = 
            begin match t.t with
              | TRef t     -> t
              | TNullRef _ -> raise @@ ParseError (ofnode "null expression must use non-null reference type" (peek s'')) 
              | _          -> raise @@ ParseError (ofnode "null expression must use reference type" (peek s''))
            end in
          { t = Null (ofnode t' t) ; start = start ; length = (peek s''').start - start }, s'''
      | LBrack -> (* either empty list or array literal *)
          let s' = snd (advance s) in
          begin match (peek s').t with
            | RBrack -> (* empty list *)
                let s'' = snd (advance s') in
                let s''' = expect s'' KOf in
                let t,s'''' = parse_ty s''' in
                { t = EmptyList t ; start = start ; length = (peek s'''').start - start }, s''''
            | _ -> (* arbitrary expression starting with '[' *)
                parse_binary_expression 0 s
          end
      | _    -> parse_binary_expression 0 s
    end


  let rec parse_stmt (s : state) (indent : int) : stmt node * state =

    let rec parse_vdecl_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s',m =
        begin match (peek s).t with
          | KLet -> snd (advance s), Const
          | KMut -> snd (advance s), Mut
          | _    -> raise @@ ParseError (ofnode "Expected a declaration" (fst (advance s)))
        end in
      let id,s'' =
        begin match (peek s').t with
          | Id i -> i, snd @@ advance s'
          | _    -> raise @@ ParseError (ofnode "Declaration requires identifier" (fst @@ advance s'))
        end in
      let t,s''' =
        if (peek s'').t = Colon then
          let t,s''' = parse_ty (snd (advance s'')) in Some t, s'''
        else None, s'' in
      let s'''' = expect s''' Token.Assign in
      let e,s''''' = parse_exp s'''' in
      { t = VDecl (id, m, t, e) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_expr_or_assn_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let l,s' = parse_exp s in
      if (peek s').t = Token.Assign then
        let _,s'' = advance s' in
        let r,s''' = parse_exp s'' in
        { t = Assn (l,r) ; start = start ; length = (peek s''').start - start }, s'''
      else
        { t = Expr l ; start = start ; length = (peek s').start - start }, s'

    and parse_if_stmt (s : state) (indent : int) (ifkey : Token.token) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s ifkey in
      let e,s'' = parse_exp s' in
      let b,s''' = parse_block s'' (indent+1) in
      if matches_indent_and s''' indent Token.KElif then
        let _,s'''' = advance s''' in
        let es,s''''' = parse_if_stmt s'''' indent (Token.KElif) in
        { t = If (e, b, [es]) ; start = start ; length = (peek s''''').start - start }, s'''''
      else if matches_indent_and s''' indent Token.KElse then
        let _,s'''' = advance (snd (advance s''')) in
        let n,s''''' = parse_block s'''' (indent+1) in
        { t = If (e, b, n) ; start = start ; length = (peek s''''').start - start }, s'''''
      else
        { t = If (e, b, []) ; start = start ; length = (peek s''').start - start }, s'''

    and parse_denull_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KDenull in
      let id,s'' =
        begin match (peek s').t with
          | Id id -> id, snd @@ advance s'
          | _     -> raise @@ ParseError (ofnode "Expected an identifier" (fst @@ advance s'))
        end in
      let s''' = expect s'' Token.Assign in
      let e,s'''' = parse_exp s''' in
      let b,s''''' = parse_block s'''' (indent+1) in
      if matches_indent_and s''''' indent Token.KElse then
        let s'''''' = snd @@ advance @@ snd @@ advance s''''' in
        let b',s''''''' = parse_block s'''''' (indent+1) in
        { t = Denull (id, e, b, b') ; start = start ; length = (peek s''''''').start - start }, s'''''''
      else
        { t = Denull (id, e, b, []) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_while_stmt (s : state) (indent : int) : stmt node * state = 
      let start = (peek s).start in
      let s' = expect s Token.KWhile in
      let c,s'' = parse_exp s' in
      let b,s''' = parse_block s'' (indent+1) in
      { t = While (c,b) ; start = start ; length = (peek s''').start - start }, s'''

    and parse_dowhile_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KDo in
      let b,s'' = parse_block s' (indent + 1) in
      let s''' = expect s'' (Token.Whitespace indent) in
      let s'''' = expect s''' Token.KWhile in
      let c,s''''' = parse_exp s'''' in
      { t = DoWhile (c,b) ; start = start ; length = (peek s''''').start - start }, s'''''

    and parse_for_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KFor in
      let id,s'' =
        begin match (peek s').t with
          | Token.Id i -> i, snd @@ advance s'
          | _          -> raise @@ ParseError (ofnode "For loop requires loop variable" (fst @@ advance s'))
        end in
      let s''' = expect s'' Token.Assign in
      let startexp,s'''' = parse_exp s''' in
      let incl1,incl2,s''''' =
        begin match (peek s'''').t with
          | Token.Dots        -> Incl, Incl, snd @@ advance s''''
          | Token.DotsPipe    -> Incl, Excl, snd @@ advance s''''
          | Token.PipeDots    -> Excl, Incl, snd @@ advance s''''
          | Token.PipeDotPipe -> Excl, Excl, snd @@ advance s''''
          | _              -> raise @@ ParseError (ofnode "expect a range indicator in for header" (fst @@ advance s''''))
        end in
      let endexp,s'''''' = parse_exp s''''' in
      let b,s''''''' = parse_block s'''''' (indent+1) in
      { t = For (id,startexp,incl1,incl2,endexp,b) ; start = start ; length = (peek s''''''').start - start }, s'''''''

    and parse_return_stmt (s : state) (indent : int) : stmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KReturn in
      let t,_ = advance s' in
      begin match t.t with
        | Token.EOF | Token.Whitespace _ ->
            { t = Return None ; start = start ; length = (peek s').start - start }, s'
        | _ ->
            let e,s'' = parse_exp s' in
            { t = Return (Some e) ; start = start ; length = (peek s'').start - start }, s''
      end
    in

    let s' = expect s (Token.Whitespace indent) in
    begin match (peek s').t with
      | Token.KLet | Token.KMut -> parse_vdecl_stmt        s' indent
      | Token.KIf               -> parse_if_stmt           s' indent Token.KIf
      | Token.KDenull           -> parse_denull_stmt       s' indent
      | Token.KWhile            -> parse_while_stmt        s' indent
      | Token.KDo               -> parse_dowhile_stmt      s' indent
      | Token.KFor              -> parse_for_stmt          s' indent
      | Token.KReturn           -> parse_return_stmt       s' indent
      | _                       -> parse_expr_or_assn_stmt s' indent
    end

  and parse_block (s : state) (indent : int) : stmt node list * state =
    if (peek s).t = Whitespace indent then
      let st,s' = parse_stmt s indent in
      let b,s'' = parse_block s' indent in
      st :: b, s''
    else
      [], s

  let parse_gstmt (s:state) : gstmt node * state =
    let rec parse_gvdecl (s:state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KGlobal in
      let s'',m =
        if (peek s').t = KMut then snd (advance s'), Mut
        else s', Const in
      let id,s''' =
        begin match (peek s'').t with
          | Id id -> id, snd (advance s'')
          | _     -> raise @@ ParseError (ofnode "Declaration requires identifier" (fst (advance s'')))
        end in
      let t,s'''' =
        if (peek s''').t = Colon then
          let t,s'''' = parse_ty (snd (advance s''')) in Some t, s''''
        else
          None, s''' in
      let s''''' = expect s'''' Token.Assign in
      let e,s'''''' = parse_exp s''''' in
      { t = GVDecl (id, m, t, e) ; start = start ; length = (peek s'''''').start - start }, s''''''

    and parse_gfdecl (s:state) : gstmt node * state =
      let start = (peek s).start in
      let s' = expect s Token.KFn in
      let id,s'' =
        begin match advance s' with
          | {t=Token.Id i;start=_;length=_},s'' -> i,s''
          | t,s''                               -> raise @@ ParseError (ofnode "Function declaration requires identifier" t)
        end in
      let arglist, s''' =
        begin match advance s'' with
          | {t=Token.Arrow;start=_;length=_}, s''' -> [], s''
          | {t=Token.Colon;start=_;length=_}, s''' ->
              let rec args (s:state) : (string * ty node) list * state =
                let id,s' = begin match advance s with
                              | {t=Token.Id i;start=_;length=_},s' -> i,s'
                              | _                                  -> raise @@ ParseError (ofnode "Expected a function argument name" (peek s))
                            end in
                let s'' = expect s' Token.Colon in
                let t,s''' = parse_ty s'' in
                if (peek s''').t = Token.Comma then
                  let _,s'''' = advance s''' in
                  let l,s''''' = args s'''' in
                  (id, t) :: l, s'''''
                else
                  [ id, t ], s'''
              in
              args s'''
          | _ -> raise @@ ParseError (ofnode "Expected a separator in function argument list" (peek s))
        end in
      let s'''' = expect s''' Token.Arrow in
      let rt,s''''' = parse_retty s'''' in
      let b,s'''''' = parse_block s''''' 1 in
      { t = GFDecl (id,arglist,rt,b) ; start = start ; length = (peek s'''''').start - start }, s''''''
    in
    let s' = expect s (Token.Whitespace 0) in
    begin match (peek s').t with
      | Token.KGlobal -> parse_gvdecl s'
      | Token.KFn     -> parse_gfdecl s'
      | _             -> raise @@ ParseError (ofnode "Expected a global variable or function declaration" (peek s'))
    end
  
  let rec parse (s : state) : gstmt node list =
    begin match s with
      | [] | [{t=Token.EOF;start=_;length=_}] -> []
      | s                -> let gs,s' = parse_gstmt s in gs :: parse s'
    end

end