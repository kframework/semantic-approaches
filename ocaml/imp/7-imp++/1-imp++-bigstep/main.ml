open Bigstep;;

let eval_aexp aexp state ins =
  let initial_cfg = AExpCfg (aexp, state, ins) in
  let final_cfgs = eval initial_cfg in
  print_cfg initial_cfg;
  print_string "\n  ---evals to--->\n";
  print_cfg_list final_cfgs;
  print_string "\n\n"
;;

let run stmt state ins =
  let stmt_desugared = desugar stmt in
  let initial_cfg = StmtCfg (stmt_desugared, state, ins) in
  let final_cfgs = eval initial_cfg in
  print_string "original program:\n";
  print_stmt stmt;
  print_string "desugared program:\n";
  print_stmt stmt_desugared;
  print_cfg initial_cfg;
  print_string "\n  ---evals to--->\n";
  print_cfg_list final_cfgs;
  print_string "\n\n"
;;



(* Parsing and testing *)

let parse_stmt file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
    Parser.main Lexer.token lexbuf
;;

(* Read a list of integers from a file
 * with each integer in a line.
 *)
let read_int_list file =
  let ins = ref [] in
  let inx = open_in file in
  try
    while true; do
      ins := (int_of_string (input_line inx)) :: !ins
    done; !ins
  with End_of_file -> 
  close_in inx;
  List.rev !ins
;;

let run_dtest dtest =
  let dtest_in = dtest ^ ".in" in
  let dtest_out = dtest ^ ".out" in
  let ins = read_int_list dtest_in in
  let outs = read_int_list dtest_out in
  let stmt = parse_stmt dtest in
  run stmt [] ins
;;




(* Some shortcut functions for ASTs *)
let aexp_plus e1 e2 = PlusAExp (e1, e2);;
let aexp_div e1 e2 = DivAExp (e1, e2);;
let bexp_not e = NotBExp (e);;
let bexp_lessthan e1 e2 = LessThanBExp (e1, e2);;
let stmt_assign x e = AssignStmt (x, e);;
let stmt_while cond block = WhileStmt (cond, block);;
let stmt_block block = BlockStmt (block);;
let block_stmt stmt = StmtBlock (stmt);;

let stmt_assign_n n = AssignStmt ("n", IntAExp (n));;
let stmt_assign_x n = AssignStmt ("x", IntAExp (n));;
let stmt_assign_y n = AssignStmt ("y", IntAExp (n));;
let stmt_assign_s n = AssignStmt ("s", IntAExp (n));;

let aexp_plus_s_n = PlusAExp (IdAExp ("s"), IdAExp ("n"));;
let aexp_minus_n_1 = PlusAExp (IdAExp ("n"), IntAExp (-1));;

let bexp_lessthan_n n = LessThanBExp (IdAExp ("n"), IntAExp (n));;
let bexp_lessthan_x n = LessThanBExp (IdAExp ("x"), IntAExp (n));;
let bexp_lessthan_y n = LessThanBExp (IdAExp ("y"), IntAExp (n));;
let bexp_lessthan_s n = LessThanBExp (IdAExp ("s"), IntAExp (n));;


let rec make_seqstmt stmts = match stmts with
| [] -> BlockStmt (EmptyBlock)
| stmt::rest_stmts -> SeqStmt (stmt, make_seqstmt rest_stmts)
;;



(* ++x / ++x at state x |-> 0.
 * Result A: 1 / 2 = 0
 * Result B: 2 / 1 = 2
 *)
let test_nondet_1 () = 
  let initial_state = [("x", 0)] in
  let exp = aexp_div (IncAExp "x") (IncAExp "x") in
  let ins = [] in
  print_string "test_nondet_1:\n";
  eval_aexp exp initial_state ins
;;

let test_nondet_2 () = 
  let initial_state = [("x", 0)] in
  let exp = aexp_plus (IdAExp "x") (IdAExp "x") in
  let ins = [] in
  print_string "test_nondet_2:\n";
  eval_aexp exp initial_state ins
;;

(* Tests programs *)

(* let_read_print *)
(* int i;
 * i = read();
 * print(i);
 *)

let pgm_let_read_print =
  make_seqstmt [
    (DeclarationStmt (["i"]));
    (stmt_assign "i" ReadAExp);
    (PrintStmt (IdAExp ("i")))]
;;

let test_let_read_print () =
  let initial_state = [] in
  let stmt = pgm_let_read_print in
  let ins = [42] in
  print_string "test_let_read_print:\n";
  run stmt initial_state ins
;;


(* sum_io *)
(* int i, s, n;
 * n = read();
 * while (i <= n) {
 *   s = s + ++i;
 * }
 * print(s);
 *) 

let pgm_sum_io = 
  make_seqstmt [
    (DeclarationStmt (["i"; "n"; "s"]));
    (stmt_assign "n" ReadAExp);
    (stmt_while
      (bexp_lessthan (IdAExp ("i")) (IdAExp ("n")))
      (block_stmt (stmt_assign "s" (aexp_plus (IdAExp ("s")) (IncAExp ("i"))))));
    (PrintStmt (IdAExp ("s")))]
;;

let test_sum_io () =
  let initial_state = [] in
  let stmt = pgm_sum_io in
  let ins = [100] in
  print_string "test_sum_io:\n";
  run stmt initial_state ins
;;







let main () =
  print_string "--- Welcome to Big-Step Semantics ---\n";
  print_string "---       Xiaohong Chen           ---\n\n";
  test_sum_io ();
  test_let_read_print ();
  test_nondet_1 (); 
  test_nondet_2 ();
  print_string "\n---         Bye bye               ---\n";
;;

(* Tests *)


let dtest_1 = "../../../../tests/imp/7-imp++/deterministic/dtest_1.imp";;
let ptest_1 = "ptest_1.imp"



let () = run_dtest dtest_1
;;
