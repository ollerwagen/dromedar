open Common

(* LLVM IR: Abstract Syntax Trees *)

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

type operand =
  | Id  of string
  | Gid of string
  | Str of string
  | IConst of int64
  | FConst of float
  | SConst of (llty * operand) list
  | AConst of (llty * operand) list
  | Null

type bop =
  | Add
  | FAdd
  | Sub
  | FSub
  | Mul
  | FMul
  | Div
  | FDiv
  | Rem
  | Shl
  | Shr
  | Sha
  | And
  | Or
  | Xor

type cmpop =
  | Eq
  | Neq
  | Greater
  | GreaterEq
  | Less
  | LessEq

type term =
  | Ret of (llty * operand) option
  | Br  of string
  | Cbr of operand * string * string

type cmp_op_type = | ICmp | FCmp

type instr =
  | Binop   of string        * bop * llty * operand * operand
  | Cmp     of string        * cmp_op_type * cmpop * llty * operand * operand
  | Alloca  of string        * llty
  | Load    of string        * llty * operand
  | Store   of                 llty * operand * operand
  | Call    of string option * llty * operand * (llty * operand) list
  | Gep     of string        * llty * operand * operand list
  | Bitcast of string        * llty * operand * llty
  | Phi     of string        * llty * (operand * string) list

type firstblock = instr list * term
type block = string * instr list * term

type ginstr =
  | FDecl    of string * llty * (llty * string) list * (firstblock * block list)
  | GDecl    of string * llty * operand
  | TDecl    of string * llty
  | GAssn    of string * llty * string
  | LinkVD   of string * llty
  | LinkFD   of string * llty * llty list
  
let rec size_ty (t:llty) : int =
  begin match t with
    | Void         -> 0
    | VariadicDots -> Stdlib.failwith "cannot ask for size of variadic dots type"
    | Namedt s     -> Stdlib.failwith "named types unimplemented"
    | Struct ts    -> List.fold_left (fun x s -> x + if s > 8 then s else 8) 0 @@ List.map size_ty ts
    | Array  (s,t) -> Int64.to_int s * size_ty t
    | I1 | I8      -> 1
    | I64 | Double | Ptr _ -> 8
    | Func   _     -> 0 (* functions w/o pointers shouldn't exist in AST->translation anywhere *)
  end

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

let rec print_operand (op:operand) : string =
  let escape_llvm : string -> string =
    String.fold_left
      (fun res c ->
        res ^
        begin match c with
          | '\n' -> "\\0A"
          | '\r' -> "\\0D"
          | '\t' -> "\\09"
          | '\'' -> "\\27"
          | '\"' -> "\\22"
          | '\\' -> "\\5C"
          | _    -> String.make 1 c
        end
      )
      ""
  in
  begin match op with
    | Id     id -> Printf.sprintf "%%%s" id
    | Gid    id -> Printf.sprintf "@%s" id
    | Str    s  -> Printf.sprintf "c\"%s\\00\"" (escape_llvm s)
    | IConst i  -> Printf.sprintf "%Ld" i
    | FConst f  -> Printf.sprintf "%f" f
    | SConst os -> Printf.sprintf "{%s}" (String.concat ", " (List.map (fun (t,o) -> Printf.sprintf "%s %s" (print_llty t) (print_operand o)) os))
    | AConst os -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun (t,o) -> Printf.sprintf "%s %s" (print_llty t) (print_operand o)) os))
    | Null      -> "null"
  end

let binop_to_string : (bop * string) list =
  [ Add,  "add"
  ; FAdd, "fadd"
  ; Sub,  "sub"
  ; FSub, "fsub"
  ; Mul,  "mul"
  ; FMul, "fmul"
  ; Div,  "sdiv"
  ; FDiv, "fdiv"
  ; Rem,  "srem"
  ; Shl,  "shl"
  ; Shr,  "lshr"
  ; Sha,  "ashr"
  ; And,  "and"
  ; Or,   "or"
  ; Xor,  "xor"
  ]

let icmpop_to_string : (cmpop * string) list =
  [ Eq,        "eq"
  ; Neq,       "ne"
  ; Greater,   "sgt"
  ; GreaterEq, "sge"
  ; Less,      "slt"
  ; LessEq,    "sle"
  ]

let fcmpop_to_string : (cmpop * string) list =
  [ Eq,        "oeq"
  ; Neq,       "une"
  ; Greater,   "ogt"
  ; GreaterEq, "oge"
  ; Less,      "olt"
  ; LessEq,    "ole"
  ]

let print_instr (i:instr) : string =
  begin match i with
    | Binop (wt,op,t,e1,e2) ->
        let opstr, tstr, e1s, e2s = List.assoc op binop_to_string, print_llty t, print_operand e1, print_operand e2 in
        Printf.sprintf "%%%s = %s %s %s, %s" wt opstr tstr e1s e2s
    | Cmp (wt,opt,op,t,e1,e2) ->
        let opname, opstr =
          begin match opt with
            | ICmp -> "icmp", List.assoc op icmpop_to_string
            | FCmp -> "fcmp", List.assoc op fcmpop_to_string
          end in
        let tstr, e1s, e2s = print_llty t, print_operand e1, print_operand e2 in
        Printf.sprintf "%%%s = %s %s %s %s, %s" wt opname opstr tstr e1s e2s
    | Alloca (wt,t) ->
        let tstr = print_llty t in
        Printf.sprintf "%%%s = alloca %s" wt tstr
    | Load (wt,t,e) ->
        let tstr, es = print_llty t, print_operand e in
        Printf.sprintf "%%%s = load %s, %s* %s" wt tstr tstr es
    | Store (t,e1,e2) ->
        let tstr, e1s, e2s = print_llty t, print_operand e1, print_operand e2 in
        Printf.sprintf "store %s %s, %s* %s" tstr e1s tstr e2s
    | Call (wt,rt,f,a) ->
        let tstr, fstr, args = print_llty rt, print_operand f, List.map (fun (t,e) -> Printf.sprintf "%s %s" (print_llty t) (print_operand e)) a in
        let wtstr =
          begin match wt with
            | None   -> ""
            | Some s -> Printf.sprintf "%%%s = " s
          end in
        Printf.sprintf "%scall %s %s(%s)" wtstr tstr fstr (String.concat ", " args)
    | Gep (wt,t,e,is) ->
        let dptr (t : llty) : llty =
          begin match t with
            | Ptr t -> t
            | _     -> Stdlib.failwith "expected pointer type"
          end
        in
        let dptrtstr, tstr, es, isstr = print_llty (dptr t), print_llty t, print_operand e, List.map (fun e -> Printf.sprintf "%s %s" (begin match e with | Gid _ | Id _ -> "i64" | _ -> "i32" end) (print_operand e)) is in
        Printf.sprintf "%%%s = getelementptr %s, %s %s%s" wt dptrtstr tstr es (String.concat "" (List.map (String.cat ", ") isstr))
    | Bitcast (wt,tf,o,tt) ->
        let startt, ostr, endt = print_llty tf, print_operand o, print_llty tt in
        let opcode =
          begin match tf, tt with
            | I64,    I8     -> "trunc"
            | I1,     I64    -> "sext"
            | I8,     I64    -> "sext"
            | I64,    Double -> "sitofp"
            | Double, I64    -> "fptosi"
            | Ptr _,  Ptr _  -> "bitcast"
            | Ptr _,  _      -> "ptrtoint"
            | _    ,  Ptr _  -> "inttoptr"
            | _    ,  _      -> "bitcast"
          end in
        Printf.sprintf "%%%s = %s %s %s to %s" wt opcode startt ostr endt
    | Phi (wt,t,os) ->
        Printf.sprintf "%%%s = phi %s %s" wt (print_llty t) (String.concat ", " (List.map (fun (op,l) -> Printf.sprintf "[%s, %%%s]" (print_operand op) l) os))
  end

let print_term (t:term) : string =
  begin match t with
    | Ret None ->
        "ret void"
    | Ret (Some (t,e)) ->
        let tstr, es = print_llty t, print_operand e in
        Printf.sprintf "ret %s %s" tstr es
    | Br l ->
        Printf.sprintf "br label %%%s" l
    | Cbr (o,l1,l2) ->
        let os = print_operand o in
        Printf.sprintf "br i1 %s, label %%%s, label %%%s" os l1 l2
  end

let print_ginstr (gi:ginstr) : string =
  begin match gi with
    | FDecl (id,rt,a,(fb,bs)) ->
        Printf.sprintf "define %s @%s(%s) {\n%s%s}\n"
          (print_llty rt)
          id
          (String.concat ", " (List.map (fun (t,e) -> Printf.sprintf "%s %%%s" (print_llty t) e) a))
          (Printf.sprintf "%s%s" (String.concat "" (List.map (fun i -> Printf.sprintf "  %s\n" (print_instr i)) (fst fb))) (Printf.sprintf "  %s\n" (print_term (snd fb))))
          (String.concat "" (List.map (fun (l,is,t) -> Printf.sprintf "%s:\n%s%s" l (String.concat "" (List.map (fun i -> Printf.sprintf "  %s\n" (print_instr i)) is)) (Printf.sprintf "  %s\n" (print_term t))) bs))
    | GDecl (id,t,init) ->
        Printf.sprintf "@%s = global %s %s\n"
          id
          (print_llty t)
          (print_operand init)
    | TDecl (id,t) -> Printf.sprintf "%s = type %s\n" (print_llty (Namedt id)) (print_llty t)
    | GAssn (t,llt,f) -> Printf.sprintf "@%s = global %s* @%s\n" t (print_llty llt) f
    | LinkVD (id,llt) -> Printf.sprintf "@%s = external global %s\n" id (print_llty llt)
    | LinkFD (id,rt,a) -> Printf.sprintf "declare %s @%s(%s)\n" (print_llty rt) id (String.concat ", " (List.map print_llty a))
  end

let print_llprog (p : ginstr list) : string =
  (readall "cutils/intrinsics.ll") ^ "\n\n" ^
  (String.concat "\n" (List.map print_ginstr p))