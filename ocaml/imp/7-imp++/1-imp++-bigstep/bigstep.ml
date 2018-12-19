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

let rec in_cfg_list cfg cfglist =
  match cfglist with
  | [] -> false
  | c::cs -> if cfg = c then true else in_cfg_list cfg cs
;;

(* Compute the union of two cfg sets, i.e., lists
 * with no duplicates.
 *)
let rec union_cfg_list cfgset1 cfgset2 =
  match cfgset1 with
  | [] -> cfgset2
  | c::cs -> 
      if in_cfg_list c cfgset2 
      then union_cfg_list cs cfgset2 
      else c::(union_cfg_list cs cfgset2)
;;



(* Prettyprinting *)

let rec string_of_id_list xs = match xs with
| [] -> ".Ids"
| [x] -> x
| x::rest_xs -> x ^ "," ^ (string_of_id_list rest_xs)
;;

let rec string_of_buffer buffer = match buffer with
| [] -> "EmptyBuffer"
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
| [] -> "EmptyState"
| [(x, n)] -> x ^ "|->" ^ (string_of_int n)
| (x, n)::rest -> x ^ "|->" ^ (string_of_int n) ^ "," ^ (string_of_state rest)
;;


let string_of_cfg cfg = match cfg with
| AExpCfg (e, s, ins) -> 
    "<" ^ (string_of_aexp e) ^ "," ^ (string_of_state s) 
    ^ "," ^ (string_of_buffer ins) ^ ">"
| BExpCfg (e, s, ins) -> 
    "<" ^ (string_of_bexp e) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| BlockCfg (block, s, ins) ->
    "<" ^ (string_of_block block) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| StmtCfg (stmt, s, ins) ->
    "<" ^ (string_of_stmt stmt) ^ "," 
    ^ (string_of_state s) ^ "," ^ (string_of_buffer ins) ^ ">"
| IntCfg (n, s, ins) ->
    "<" ^ (string_of_int n) ^ "," ^ (string_of_state s) ^
    "," ^ (string_of_buffer ins) ^ ">"
| BoolCfg (b, s, ins) ->
    "<" ^ (string_of_bool b) ^ (string_of_state s) ^ (string_of_buffer ins) ^ ">"
| StateCfg (s, ins, outs) ->
    "<" ^ (string_of_state s) ^ "," 
    ^ (string_of_buffer ins) ^ "," ^ (string_of_buffer outs) ^ ">"
| ErrCfg (s, ins) ->
    "<" ^ "error" ^ "," ^ (string_of_state s) ^ "," ^ (string_of_buffer ins) ^ ">"
| HaltingCfg (s, ins, outs) ->
    "<" ^ "halting" ^ "," ^ (string_of_state s) ^ "," ^ (string_of_buffer ins)
    ^ "," ^ (string_of_buffer outs) ^ ">"
;;

let print_stmt stmt = print_string ((string_of_stmt stmt) ^ "\n");;
let print_cfg cfg = print_string ((string_of_cfg cfg) ^ "\n");;
let rec print_cfg_list cfgs = match cfgs with
| [] -> print_string "no more configuration.\n\n"
| cfg::cfgs_rest -> print_cfg cfg; print_cfg_list cfgs_rest
;;


exception EvalError of cfg * string
exception SimpleEvalError


(* Big-Step Evaluation Function - Choice Nondeterminism *)

let rec eval cfg = match cfg with
(* Evaluate expressions *)
| AExpCfg (IntAExp (n), s, ins) -> [IntCfg (n, s, ins)]
| AExpCfg (IdAExp (x), s, ins) -> 
    (match lookup_state s x with
    | Some (n) -> [IntCfg (n, s, ins)]
    | None -> raise SimpleEvalError
    )
| AExpCfg (PlusAExp (e1, e2), s, ins) ->
    let cfgs_e1_e2 = 
      let eval_e2 cfg_after_e1 = match cfg_after_e1 with
      | ErrCfg (s1, ins1) -> [ErrCfg (s1, ins1)]
      | IntCfg (n1, s1, ins1) -> 
          let construct_final_cfg cfg_after_e2 = match cfg_after_e2 with
          | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
          | IntCfg (n2, s2, ins2) -> IntCfg (n1 + n2, s2, ins2)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e2, s1, ins1)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e2 (eval (AExpCfg (e1, s, ins))))
    in
    let cfgs_e2_e1 = 
      let eval_e1 cfg_after_e2 = match cfg_after_e2 with
      | ErrCfg (s2, ins2) -> [ErrCfg (s2, ins2)]
      | IntCfg (n2, s2, ins2) -> 
          let construct_final_cfg cfg_after_e1 = match cfg_after_e1 with
          | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
          | IntCfg (n1, s1, ins1) -> IntCfg (n1 + n2, s1, ins1)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e1, s2, ins2)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e1 (eval (AExpCfg (e2, s, ins))))
    in 
    union_cfg_list cfgs_e1_e2 cfgs_e2_e1
| AExpCfg (DivAExp (e1, e2), s, ins) ->
    let cfgs_e1_e2 = 
      let eval_e2 cfg_after_e1 = match cfg_after_e1 with
      | ErrCfg (s1, ins1) -> [ErrCfg (s1, ins1)]
      | IntCfg (n1, s1, ins1) -> 
          let construct_final_cfg cfg_after_e2 = match cfg_after_e2 with
          | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
          | IntCfg (n2, s2, ins2) ->
              if n2 <> 0 then IntCfg (n1 / n2, s2, ins2) else ErrCfg (s2, ins2)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e2, s1, ins1)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e2 (eval (AExpCfg (e1, s, ins))))
    in
    let cfgs_e2_e1 = 
      let eval_e1 cfg_after_e2 = match cfg_after_e2 with
      | ErrCfg (s2, ins2) -> [ErrCfg (s2, ins2)]
      | IntCfg (n2, s2, ins2) -> 
          let construct_final_cfg cfg_after_e1 = match cfg_after_e1 with
          | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
          | IntCfg (n1, s1, ins1) ->
              if n2 <> 0 then IntCfg (n1 / n2, s1, ins1) else ErrCfg (s1, ins1)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e1, s2, ins2)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e1 (eval (AExpCfg (e2, s, ins))))
    in 
    union_cfg_list cfgs_e1_e2 cfgs_e2_e1
| AExpCfg (IncAExp (x), s, ins) ->
    (match lookup_state s x with
    | Some (n) -> [IntCfg (n + 1, (update_state s (n + 1) x), ins)]
    | None -> raise SimpleEvalError 
    )
| AExpCfg (ReadAExp, s, []) -> raise (EvalError (cfg, "Out of inputs!"))
| AExpCfg (ReadAExp, s, (input::inputs)) -> [IntCfg (input, s, inputs)]
| BExpCfg (BoolBExp (b), s, ins) -> [BoolCfg(b, s, ins)]
| BExpCfg (LessThanBExp (e1, e2), s, ins) ->
    let cfgs_e1_e2 = 
      let eval_e2 cfg_after_e1 = match cfg_after_e1 with
      | ErrCfg (s1, ins1) -> [ErrCfg (s1, ins1)]
      | IntCfg (n1, s1, ins1) -> 
          let construct_final_cfg cfg_after_e2 = match cfg_after_e2 with
          | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
          | IntCfg (n2, s2, ins2) -> BoolCfg (n1 < n2, s2, ins2)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e2, s1, ins1)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e2 (eval (AExpCfg (e1, s, ins))))
    in
    let cfgs_e2_e1 = 
      let eval_e1 cfg_after_e2 = match cfg_after_e2 with
      | ErrCfg (s2, ins2) -> [ErrCfg (s2, ins2)]
      | IntCfg (n2, s2, ins2) -> 
          let construct_final_cfg cfg_after_e1 = match cfg_after_e1 with
          | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
          | IntCfg (n1, s1, ins1) -> BoolCfg (n1 < n2, s1, ins1)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (AExpCfg (e1, s2, ins2)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e1 (eval (AExpCfg (e2, s, ins))))
    in 
    union_cfg_list cfgs_e1_e2 cfgs_e2_e1
| BExpCfg (NotBExp (e), s, ins) ->
    let construct_final_cfg cfg_after_e = match cfg_after_e with
    | ErrCfg (s1, ins1) -> ErrCfg (s1, ins1)
    | BoolCfg (b, s1, ins1) -> BoolCfg (not b, s1, ins1)
    | _ -> raise  SimpleEvalError
    in
    List.map construct_final_cfg (eval (BExpCfg (e, s, ins)))
| BExpCfg (AndBExp (e1, e2), s, ins) ->
    (* correct implementation of short-circuiting *)
    let cfgs_e1_e2 = 
      let eval_e2 cfg_after_e1 = match cfg_after_e1 with
      | ErrCfg (s1, ins1) -> [ErrCfg (s1, ins1)]
      | BoolCfg (b1, s1, ins1) -> 
          let construct_final_cfg cfg_after_e2 = match cfg_after_e2 with
          | ErrCfg (s2, ins2) -> ErrCfg (s2, ins2)
          | BoolCfg (b2, s2, ins2) -> BoolCfg (b1 && b2, s2, ins2)
          | _ -> raise SimpleEvalError
          in
          List.map construct_final_cfg (eval (BExpCfg (e2, s1, ins1)))
      | _ -> raise SimpleEvalError
      in
      List.flatten (List.map eval_e2 (eval (BExpCfg (e1, s, ins))))
    in
    cfgs_e1_e2
(* Execute statements and blocks *)
| BlockCfg (EmptyBlock, s, ins) -> [StateCfg (s, ins, [])]
| BlockCfg (StmtBlock (stmt), s, ins) -> eval (StmtCfg (stmt, s, ins))
| StmtCfg (BlockStmt (block), s, ins) -> eval (BlockCfg (block, s, ins))
| StmtCfg (AssignStmt (x, e), s, ins) ->
    let construct_final_cfg cfg_after_e = match cfg_after_e with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | IntCfg (n, s1, ins1) -> StateCfg ((update_state s1 n x), ins1, [])
    | _ -> raise SimpleEvalError
    in
    List.map construct_final_cfg (eval (AExpCfg (e, s, ins)))
(* Big-step semantics is desperate in capturing concurrent behavior,
 * such as spawn. Here is a highly deterministic definition, where
 * the execution of new threads are postponed as long as possible.
 *)
| StmtCfg (SpawnStmt (block), s, ins) ->
    eval (BlockCfg (block, s, ins))
| StmtCfg (SeqStmt (SpawnStmt (block), stmt), s, ins) ->
    (* First evaluate stmt, and then block. *)
    let eval_block cfg_after_stmt = match cfg_after_stmt with
    | HaltingCfg (s1, ins1, outs1) -> [HaltingCfg (s1, ins1, outs1)]
    | StateCfg (s1, ins1, outs1) -> 
        let construct_final_cfg cfg_after_block = match cfg_after_block with
        | HaltingCfg (s2, ins2, outs2) -> HaltingCfg (s1, ins2, outs2 @ outs1)
        | StateCfg (s2, ins2, outs2) -> StateCfg (s2, ins2, outs2 @ outs1)
        | _ -> raise SimpleEvalError
        in
        List.map construct_final_cfg (eval (BlockCfg (block, s1, ins1)))
    | _ -> raise SimpleEvalError
    in
    List.flatten (List.map eval_block (eval (StmtCfg (stmt, s, ins))))
| StmtCfg (SeqStmt (stmt1, stmt2), s, ins) ->
    let eval_stmt2 cfg_after_stmt1 = match cfg_after_stmt1 with
    | HaltingCfg (s1, ins1, outs1) -> [HaltingCfg (s1, ins1, outs1)]
    | StateCfg (s1, ins1, outs1) -> 
        let construct_final_cfg cfg_after_stmt2 = match cfg_after_stmt2 with
        | HaltingCfg (s2, ins2, outs2) -> HaltingCfg (s2, ins2, outs1 @ outs2)
        | StateCfg (s2, ins2, outs2) -> StateCfg (s2, ins2, outs1 @ outs2)
        | _ -> raise SimpleEvalError
        in 
        List.map construct_final_cfg (eval (StmtCfg (stmt2, s1, ins1)))
    | _ -> raise SimpleEvalError
    in
    List.flatten (List.map eval_stmt2 (eval (StmtCfg (stmt1, s, ins))))
| StmtCfg (IfStmt (cond, block1, block2), s, ins) ->
    let eval_block cfg_after_cond = match cfg_after_cond with
    | ErrCfg (s1, ins1) -> [HaltingCfg (s1, ins1, [])]
    | BoolCfg (b, s1, ins1) ->
        if b then eval (BlockCfg (block1, s1, ins1))
        else eval (BlockCfg (block2, s1, ins1))
    | _ -> raise SimpleEvalError
    in
    List.flatten (List.map eval_block (eval (BExpCfg (cond, s, ins))))
| StmtCfg (WhileStmt (cond, block), s, ins) ->
    let eval_block cfg_after_cond = match cfg_after_cond with
    | ErrCfg (s1, ins1) -> [HaltingCfg (s1, ins1, [])]
    | BoolCfg (b, s1, ins1) -> 
        if b = false then [StateCfg (s1, ins1, [])]
        else let while_unfold = SeqStmt (BlockStmt (block), WhileStmt (cond, block)) in
          eval (StmtCfg (while_unfold, s1, ins1))
    | _ -> raise SimpleEvalError
    in
    List.flatten (List.map eval_block (eval (BExpCfg (cond, s, ins))))
| StmtCfg (PrintStmt (e), s, ins) ->
    let construct_final_cfg cfg_after_e = match cfg_after_e with
    | ErrCfg (s1, ins1) -> HaltingCfg (s1, ins1, [])
    | IntCfg (n, s1, ins1) -> StateCfg (s1, ins1, [n])
    | _ -> raise SimpleEvalError
    in
    List.map construct_final_cfg (eval (AExpCfg (e, s, ins)))
| StmtCfg (HaltStmt, s, ins) -> [HaltingCfg (s, ins, [])]
| StmtCfg(LetStmt (x, e, stmt), s, ins) ->
    (* In practice, e will only be IntAExp (0). *)
    let eval_stmt cfg_after_e = match cfg_after_e with
    | ErrCfg (s1, ins1) -> [StateCfg (s1, ins1, [])] (* unreachable *)
    | IntCfg (n, s1, ins1) -> 
        let s1_upd = update_state s1 n x in
        let construct_final_cfg cfg_after_stmt = match cfg_after_stmt with
        | HaltingCfg (s2, ins2, outs2) ->
            let s2_restore = lookup_and_update_state s2 s1 x in
            StateCfg (s2_restore, ins2, outs2)
        | StateCfg (s2, ins2, outs2) ->
            let s2_restore = lookup_and_update_state s2 s1 x in
            StateCfg (s2_restore, ins2, outs2)
        | _ -> raise SimpleEvalError
        in
        List.map construct_final_cfg (eval (StmtCfg (stmt, s1_upd, ins1)))
    | _ -> raise SimpleEvalError
    in
    List.flatten (List.map eval_stmt (eval (AExpCfg (e, s, ins))))
| _ -> raise SimpleEvalError
;;

(*

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

*)
