(* Big-Step Semantics of IMP++ *)

open List;;

(* Abstract Syntax Tree of IMP++ *)

type id = string
;;
type aexp =
  | IntAExp of int
  | IdAExp of id
  | PlusAExp of aexp * aexp
  | DivAExp of aexp * aexp
  (* IMP++ *)
  | IncAExp of id
  | ReadAExp
;;
type bexp =
  | BoolBExp of bool
  | LessThanBExp of aexp * aexp
  | NotBExp of bexp
  | AndBExp of bexp * bexp
;;
type block =
  | EmptyBlock
  | StmtBlock of stmt
and stmt =
  | BlockStmt of block
  | AssignStmt of id * aexp
  | SeqStmt of stmt * stmt
  | IfStmt of bexp * block * block
  | WhileStmt of bexp * block
  (* IMP++ *)
  | PrintStmt of aexp
  | HaltStmt
  | SpawnStmt of block
  | DeclarationStmt of (id list)
  (* An auxiliary construct used to define declaration statements *)
  | LetStmt of id * aexp * stmt
;;
(* IMP++ *)
(* We don't use pgm anymore. We only use stmt. *)
type pgm = stmt
;;

(* Reorder (s1 s2) s3 to s1 (s2 s3) *)

let rec reorder_seq stmt = 
  let (s, should_repeat) = reorder_seq_inner stmt in
  if should_repeat then reorder_seq s else s
and reorder_seq_inner stmt =  match stmt with
| BlockStmt (b) -> (BlockStmt (reorder_seq_in_block b), false)
| SeqStmt (SeqStmt (s1, s2), s) -> (SeqStmt (s1, SeqStmt (s2, s)), true)
| SeqStmt (s1, s) -> (SeqStmt (s1, reorder_seq s), false)
| IfStmt (cond, b1, b2) -> 
    let b1_reordered = reorder_seq_in_block b1 in
    let b2_reordered = reorder_seq_in_block b2 in
    (IfStmt (cond, b1_reordered, b2_reordered), false)
| WhileStmt (cond, b) -> 
    let b_reordered = reorder_seq_in_block b in
    (WhileStmt (cond, b_reordered), false)
| SpawnStmt (b) -> 
    let b_reordered = reorder_seq_in_block b in
    (SpawnStmt (b_reordered), false)
| _ -> (stmt, false)
and reorder_seq_in_block b = match b with
| EmptyBlock -> EmptyBlock
| StmtBlock (s) -> StmtBlock (reorder_seq s)
;;


(* A desugaring function that translates declarations to let-statements. *)

let rec construct_lets xl s = match xl with
| [] -> s
| (x::xs) -> LetStmt (x, IntAExp (0), construct_lets xs s)
;;

let rec desugar stmt = let stmt_reordered = reorder_seq stmt in
  match stmt_reordered with
| BlockStmt (b) -> BlockStmt (desugar_in_block b)
| SeqStmt (DeclarationStmt (xl), s) -> construct_lets xl s
| IfStmt (cond, b1, b2) -> 
    let b1_desugared = desugar_in_block b1 in
    let b2_desugared = desugar_in_block b2 in
    IfStmt (cond, b1_desugared, b2_desugared)
| WhileStmt (cond, b) -> 
    let b_desugared = desugar_in_block b in
    WhileStmt (cond, b_desugared)
| SpawnStmt (b) -> 
    let b_desugared = desugar_in_block b in
    SpawnStmt (b_desugared)
| _ -> stmt
and desugar_in_block b = match b with
| EmptyBlock -> EmptyBlock
| StmtBlock (s) -> StmtBlock (desugar s)
;;



(* Configurations *)

type state = (id * int) list
;;

let lookup_state sigma x =
  try Some(assoc x sigma) with
  | Not_found -> None
;;

let rec update_state sigma n x =
  match sigma with 
  | [] -> [(x, n)]
  | (y, m)::sigma' -> if x = y then (x, n)::sigma' else (y, m)::(update_state sigma' n x)
;;


let rec undeclare_state s x = remove_assoc x s;;

(* Return s1[s2(x)/x]. *)
let lookup_and_update_state s1 s2 x =
  match lookup_state s2 x with
  | None -> undeclare_state s1 x
  | Some (n) -> update_state s1 n x
;;

let rec init_state xs n =
  match xs with
  | [] -> []
  | x::xs -> (x,n)::(init_state xs n)
;;

type buffer = int list
;;

type cfg =
  (* Non-termination configurations *)
  | AExpCfg of aexp * state * buffer
  | BExpCfg of bexp * state * buffer
  | BlockCfg of block * state * buffer
  | StmtCfg of stmt * state * buffer
  (* Termination configurations *)
  | IntCfg of int * state * buffer
  | BoolCfg of bool * state * buffer
  | StateCfg of state * buffer * buffer (* (state,inputs,outputs) *)
  (* Error configurations *)
  | ErrCfg of state * buffer (* division-by-zero *)
  (* Halting configurations *)
  | HaltingCfg of state * buffer * buffer (* (state,inputs,outputs) *)
;;


(* Prettyprinting *)

let rec string_of_id_list xs = match xs with
| [] -> ".Ids"
| [x] -> x
| x::rest_xs -> x ^ "," ^ (string_of_id_list rest_xs)
;;

let rec string_of_buffer buffer = match buffer with
| [] -> ".Buffer"
| [n] -> string_of_int n
| n::ns -> (string_of_int n) ^ "::" ^ (string_of_buffer ns)
;;

let rec string_of_aexp aexp = match aexp with
| IntAExp (n) -> string_of_int n
| IdAExp (x) -> x
| PlusAExp (e1, e2) -> (string_of_aexp e1) ^ "+" ^ (string_of_aexp e2)
| DivAExp(e1, e2) -> (string_of_aexp e1) ^ "/" ^ (string_of_aexp e2)
| IncAExp (x) -> "++" ^ x
| ReadAExp -> "read()"
;;

let rec string_of_bexp bexp = match bexp with
| BoolBExp (b) -> string_of_bool b
| LessThanBExp (e1, e2) -> (string_of_aexp e1) ^ "<" ^ (string_of_aexp e2)
| NotBExp (e) -> "!" ^ "(" ^ (string_of_bexp e) ^ ")"
| AndBExp (e1, e2) -> (string_of_bexp e1) ^ "&&" ^ (string_of_bexp e2)
;;

let rec string_of_block b = match b with
| EmptyBlock -> "{}"
| StmtBlock (stmt) -> "{" ^ (string_of_stmt stmt) ^ "}"
and string_of_stmt stmt = match stmt with
| BlockStmt (b) -> (string_of_block b)
| AssignStmt (x, e) -> x ^ ":=" ^ (string_of_aexp e) ^ ";"
| SeqStmt (s1, s2) -> "(" ^ (string_of_stmt s1) ^ ")" 
    ^ " " ^ "(" ^ (string_of_stmt s2) ^ ")"
| IfStmt (cond, b1, b2) -> 
    "if" ^ "(" ^ (string_of_bexp cond) ^ ")" ^ (string_of_block b1)
    ^ (string_of_block b2)
| WhileStmt (cond, b) -> 
    "while" ^ "(" ^ (string_of_bexp cond) ^ ")" ^ (string_of_block b)
| PrintStmt (e) -> "print(" ^ (string_of_aexp e) ^ ");"
| HaltStmt -> "halt();"
| SpawnStmt (b) -> "spawn" ^ " " ^ (string_of_block b)
| DeclarationStmt (xs) -> "int" ^ " " ^ (string_of_id_list xs) ^ ";"
| LetStmt (x, e, stmt) -> 
    "let" ^ " " ^ x ^ "=" ^ (string_of_aexp e) ^ " "
    ^ "in" ^ " (" ^ (string_of_stmt stmt) ^ ")"
;;

let rec string_of_state s = match s with
| [] -> "emptystate"
| [(x, n)] -> x ^ "|->" ^ (string_of_int n)
| (x, n)::rest -> x ^ "|->" ^ (string_of_int n) ^ "," ^ (string_of_state rest)
;;


let string_of_cfg cfg = match cfg with
| AExpCfg (e, s, ins) -> 
    "<" ^ (string_of_aexp e) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| BExpCfg (e, s, ins) -> 
    "<" ^ (string_of_bexp e) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| BlockCfg (block, s, ins) ->
    "<" ^ (string_of_block block) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| StmtCfg (stmt, s, ins) ->
    "<" ^ (string_of_stmt stmt) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| IntCfg (n, s, ins) ->
    "<" ^ (string_of_int n) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| BoolCfg (b, s, ins) ->
    "<" ^ (string_of_bool b) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| StateCfg (s, ins, outs) ->
    "<" ^ (string_of_state s) ^ (string_of_buffer ins) ^ (string_of_buffer outs) ^ ">"
| ErrCfg (s, ins) ->
    "<" ^ "error" ^ "," ^ (string_of_state s) ^ "," ^ (string_of_buffer ins) ^ ">"
| HaltingCfg (s, ins, outs) ->
    "<" ^ "halting" ^ "," ^ (string_of_state s) ^ "," ^ (string_of_buffer ins)
    ^ "," ^ (string_of_buffer outs) ^ ">"
;;

let print_stmt stmt = print_string ((string_of_stmt stmt) ^ "\n");;
let print_cfg cfg = print_string ((string_of_cfg cfg) ^ "\n");;



(* Big-Step Evaluation Function *)

let rec eval cfg = match cfg with
(* Evaluate expressions *)
| AExpCfg (IntAExp (n), s, ins) -> IntCfg (n, s, ins)
| AExpCfg (IdAExp (x), s, ins) -> 
    (match lookup_state s x with
    | Some (n) -> IntCfg (n, s, ins)
    | None -> cfg
    )
| AExpCfg (PlusAExp (e1, e2), s, ins) ->
    (* May lose nondeterministic behavior. *)
    (match eval (AExpCfg (e1, s, ins)) with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | IntCfg (n1, s1, ins1) ->
        (match eval (AExpCfg (e2, s1, ins1)) with
        | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
        | IntCfg (n2, s2, ins2) -> IntCfg (n1 + n2, s2, ins2)
        | _ -> cfg
        )
    | _ -> cfg
    )
| AExpCfg (DivAExp (e1, e2), s, ins) ->
    (* May lose nondeterministic behavior. *)
    (match eval (AExpCfg (e1, s, ins)) with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | IntCfg (n1, s1, ins1) ->
        (match eval (AExpCfg (e2, s1, ins1)) with
        | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
        | IntCfg (n2, s2, ins2) -> 
            if n2 <> 0 then IntCfg (n1 + n2, s2, ins2) else ErrCfg (s2, ins2)
        | _ -> cfg
        )
    | _ -> cfg
    )
| AExpCfg (IncAExp (x), s, ins) ->
    (match lookup_state s x with
    | Some (n) -> IntCfg (n + 1, (update_state s (n + 1) x), ins)
    | None -> cfg
    )
| AExpCfg (ReadAExp, s, []) -> cfg
| AExpCfg (ReadAExp, s, (input::inputs)) -> IntCfg (input, s, inputs)
| BExpCfg (BoolBExp (b), s, ins) -> BoolCfg(b, s, ins)
| BExpCfg (LessThanBExp (e1, e2), s, ins) ->
    (* May lose nondeterministic behavior. *)
    (match eval (AExpCfg (e1, s, ins)) with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | IntCfg (n1, s1, ins1) -> 
        (match eval (AExpCfg (e2, s1, ins1)) with
        | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
        | IntCfg (n2, s2, ins2) -> BoolCfg (n1 < n2, s2, ins2)
        | _ -> cfg
        )
    | _ -> cfg
    )
| BExpCfg (NotBExp (e), s, ins) -> 
    (match eval (BExpCfg (e, s, ins)) with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | BoolCfg (b, s1, ins1) -> BoolCfg (not b, s1, ins1)
    | _ -> cfg
    )
| BExpCfg (AndBExp (e1, e2), s, ins) ->
    (* correct implementation of short-circuiting *)
    (match eval (BExpCfg (e1, s, ins)) with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | BoolCfg (b1, s1, ins1) ->
        if b1 = false then BoolCfg (false, s1, ins1) else
          (match eval (BExpCfg (e2, s1, ins1)) with
          | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
          | BoolCfg (b2, s2, ins2) -> BoolCfg (b1 && b2, s2, ins2)
          | _ -> cfg
          )
    | _ -> cfg
    )

(* Execute statements and blocks *)
| BlockCfg (EmptyBlock, s, ins) -> StateCfg (s, ins, [])
| BlockCfg (StmtBlock (stmt), s, ins) -> eval (StmtCfg (stmt, s, ins))
| StmtCfg (BlockStmt (block), s, ins) -> eval (BlockCfg (block, s, ins))
| StmtCfg (AssignStmt (x, e), s, ins) ->
    (match eval (AExpCfg (e, s, ins)) with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | IntCfg (n, s1, ins1) -> StateCfg ((update_state s1 n x), ins1, [])
    | _ -> cfg
    )
| StmtCfg (SeqStmt (stmt1, stmt2), s, ins) ->
    (match eval (StmtCfg (stmt1, s, ins)) with
    | HaltingCfg (s1, ins1, outs1) -> HaltingCfg (s1, ins1, outs1)
    | StateCfg (s1, ins1, outs1) ->
        (match eval (StmtCfg (stmt2, s1, ins1)) with
        | HaltingCfg (s2, ins2, outs2) -> HaltingCfg (s2, ins2, outs1 @ outs2)
        | StateCfg (s2, ins2, outs2) -> StateCfg (s2, ins2, outs1 @ outs2)
        | _ -> cfg
        )
    | err_cfg -> print_string "unexpected case (seq):\n"; print_cfg err_cfg; cfg
    )
| StmtCfg (IfStmt (cond, block1, block2), s, ins) ->
    (match eval (BExpCfg (cond, s, ins)) with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | BoolCfg (b, s1, ins1) -> 
        if b then eval (BlockCfg (block1, s1, ins1))
        else eval (BlockCfg (block2, s1, ins1))
    | _ -> cfg
    )
| StmtCfg (WhileStmt (cond, block), s, ins) ->
    (match eval (BExpCfg (cond, s, ins)) with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | BoolCfg (b, s1, ins1) ->
        if b = false then StateCfg (s1, ins1, [])
        else let while_unfold = SeqStmt (BlockStmt (block), WhileStmt (cond, block)) in
          eval (StmtCfg (while_unfold, s1, ins1))
    | err_cfg -> print_string "unexpected case while:\n"; print_cfg err_cfg; cfg
    )
| StmtCfg (PrintStmt (e), s, ins) ->
    (match eval (AExpCfg (e, s, ins)) with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | IntCfg (n, s1, ins1) -> StateCfg (s1, ins1, [n])
    | _ -> cfg
    )
| StmtCfg (HaltStmt, s, ins) -> HaltingCfg (s, ins, [])
| StmtCfg (SpawnStmt (block), s, ins) ->
    (* May lose a lot of nondeterministic behavior. *)
    eval (BlockCfg (block, s, ins))
| StmtCfg(LetStmt (x, e, stmt), s, ins) ->
    (* In practice, e will only be IntAExp (0). *)
    (match eval (AExpCfg (e, s, ins)) with
    | ErrCfg (s1, ins1) -> StateCfg (s1, ins1, []) (* unreachable *)
    | IntCfg (n, s1, ins1) -> let s1_upd = update_state s1 n x in
        (match eval (StmtCfg (stmt, s1_upd, ins1)) with
        | HaltingCfg (s2, ins2, outs2) ->
            let s2_restore = lookup_and_update_state s2 s1 x in
            StateCfg (s2_restore, ins2, outs2)
        | StateCfg (s2, ins2, outs2) ->
            let s2_restore = lookup_and_update_state s2 s1 x in
            StateCfg (s2_restore, ins2, outs2)
        | err_cfg -> print_string "unexpected case let:\n"; print_cfg err_cfg ; cfg
        )
    | _ -> cfg
    )
| _ -> cfg (* unreachable *)
;;
