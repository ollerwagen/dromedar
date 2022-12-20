open Common

(* LLVM IR: Abstract Syntax Trees *)

type llty =
  | Void
  | Namedt of string
  | Struct of llty list
  | Func of llty list * llty
  | Array of int * llty
  | I1
  | I8
  | I64
  | Double
  | Ptr of llty

type operand =
  | Id  of string
  | Gid of string
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
  | Shl
  | Shr
  | Sha
  | And
  | Or
  | Xor

type term =
  | Ret of (llty * operand) option
  | Br  of string
  | Cbr of operand * string * string

type instr =
  | Binop  of string        * bop * llty * operand * operand
  | Alloca of string        * llty
  | Load   of string        * llty * operand
  | Store  of                 llty * operand * operand
  | Call   of string option * llty * operand * (llty * operand) list
  | Gep    of string        * llty * operand * (llty * operand) list

type firstblock = instr list * term
type block = string * instr list * term

type ginstr =
  | FDecl of string * llty * (llty * string) list * (firstblock * block list)
  
let rec print_llty (t:llty) : string =
  begin match t with
    | Void         -> "void"
    | Namedt s     -> "%s"
    | Struct ts    -> Printf.sprintf "{%s}" (String.concat ", " (List.map print_llty ts))
    | Func   (a,r) -> Printf.sprintf "%s(%s)" (print_llty r) (String.concat ", " (List.map print_llty a))
    | Array  (s,t) -> Printf.sprintf "[%d x %s]" s (print_llty t)
    | I1           -> "i1"
    | I8           -> "i8"
    | I64          -> "i64"
    | Double       -> "double"
    | Ptr    t     -> Printf.sprintf "%s*" (print_llty t)
  end

let rec print_operand (op:operand) : string =
  begin match op with
    | Id     id -> Printf.sprintf "%%%s" id
    | Gid    id -> Printf.sprintf "@%s" id
    | IConst i  -> Printf.sprintf "%Ld" i
    | FConst f  -> Printf.sprintf "%f" f
    | SConst os -> Printf.sprintf "{%s}" (String.concat ", " (List.map (fun (t,o) -> Printf.sprintf "%s %s" (print_llty t) (print_operand o)) os))
    | AConst os -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun (t,o) -> Printf.sprintf "%s %s" (print_llty t) (print_operand o)) os))
    | Null      -> "null"
  end

let op_to_string : (bop * string) list =
  [ Add,  "add"
  ; FAdd, "fadd"
  ; Sub,  "sub"
  ; FSub, "fsub"
  ; Mul,  "mul"
  ; FMul, "fmul"
  ; Shl,  "shl"
  ; Shr,  "lsrh"
  ; Sha,  "ashr"
  ; And,  "and"
  ; Or,   "or"
  ; Xor,  "xor"
  ]

let print_instr (i:instr) : string =
  begin match i with
    | Binop (wt,op,t,e1,e2) ->
        let opstr, tstr, e1s, e2s = List.assoc op op_to_string, print_llty t, print_operand e1, print_operand e2 in
        Printf.sprintf "%%%s = %s %s %s, %s" wt opstr tstr e1s e2s
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
        let tstr, es, isstr = print_llty t, print_operand e, List.map (fun (t,e) -> Printf.sprintf "%s %s" (print_llty t) (print_operand e)) is in
        Printf.sprintf "getelementptr %s, %s%s" tstr es (String.concat "" (List.map (String.cat ", ") isstr))
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
  end

let print_llprog (p : ginstr list) : string =
  (readall "builtindecls.ll") ^ "\n\n" ^
  (String.concat "\n" (List.map print_ginstr p))