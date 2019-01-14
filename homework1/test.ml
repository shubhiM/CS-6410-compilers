open Sexp
open OUnit2
open Expr
open ExtLib

(* a helper for testing integers *)
let t_int name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int);;

(* A helper for testing primitive values (won't print datatypes well) *)
let t_any name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:dump);;

(* Feel free to add any new testing functions you may need *)


(* It can be useful to aggregate tests into lists if they test separate
functions, and put them together at the end *)

let env1 = [("a", 5); ("b", 15)];;

let new_env1 = (add env1 "d" 20)
let new_env2 = (add env1 "a" 10)

let add_tests = [
    t_any "add1" new_env1 [("d", 20); ("a", 5); ("b", 15)];
    t_any "add2" new_env2 [("a", 10); ("a", 5); ("b", 15)];
  ];;

let get_tests = [
  t_any "get1" (get env1 "a") (Some(5));
  t_any "get2" (get env1 "b") (Some(15));
  t_any "get3" (get env1 "d") None;
  (* More tests here *)
  t_any "get4" (get new_env2 "a") (Some(10));
  t_any "get5" (get new_env1 "d") (Some(20));
];;

let contains_tests = [
  t_any "contains1" (contains env1 "d") false;
  t_any "contains2" (contains new_env1 "d") true;
  (* More tests here *)
];;

(* Arithmetic expression examples *)
let ex1 = (Plus(Plus(Times(Plus(Num(5), Variable("y")),
                           Variable("x")),
                      Num(2)),
                Num(1)));;
let ex2 = (Times(Plus(Num(5), Num(6)),
                 Plus(Num(7), Num(8))));;
let ex3 = (Plus(Num(5), Plus(Times(Num(6), Num(7)), Num(8))));;
let ex4 = (Plus(Times(Plus(Num(5), Num(6)), Num(7)), Num(8)));;
let ex5 = (Times(Variable("x"), Variable("y")));;
let ex6 = (Plus (Variable("x"), Variable("y")));;
let ex7 = (Variable("x"));;
let ex8 = (Num(1));;

let evaluate_tests = [
  (* positive tests *)
  t_int "evaluate1" (evaluate (Times(Num(0), Num(5))) []) 0;
  t_int "evaluate2" (evaluate (Times(Num(1), Num(5))) []) 5;
  t_int "evaluate_ex_1" (evaluate ex1 [("x", 2) ; ("y", 0)]) 13;
  t_int "evaluate_ex_2" (evaluate ex2 []) 165;
  t_int "evaluate_ex_3" (evaluate ex3 []) 55;
  t_int "evaluate_ex_4" (evaluate ex4 []) 85;
  t_int "evaluate_ex_5" (evaluate ex5 [("x", 2) ; ("y", 10)]) 20;
  t_int "evaluate_ex_6" (evaluate ex6 [("x", 2) ; ("y", 10)]) 12;
  t_int "evaluate_ex_8" (evaluate ex8 []) 1;
  (* negative tests *)
];;

(* tests for pretty printing *)
let ex1_to_string = (pretty ex1);;
let ex2_to_string = (pretty ex2);;
let ex3_to_string = (pretty ex3);;
let ex4_to_string = (pretty ex4);;
let ex5_to_string = (pretty ex5);;
let ex6_to_string = (pretty ex6);;
let ex7_to_string = (pretty ex7);;
let ex8_to_string = (pretty ex8);;

let pretty_tests = [
  t_any "pretty_ex1" ex1_to_string "(5 + y)x + 2 + 1";
  t_any "pretty_ex2" ex2_to_string "(5 + 6) * (7 + 8)";
  t_any "pretty_ex3" ex3_to_string "5 + 6 * 7 + 8";
  t_any "pretty_ex4" ex4_to_string "(5 + 6) * 7 + 8";
  t_any "pretty_ex5" ex5_to_string "xy";
  t_any "pretty_ex6" ex6_to_string "x + y";
  t_any "pretty_ex7" ex7_to_string "x";
  t_any "pretty_ex8" ex8_to_string "1";
];;
 

let all_arith_tests =
  add_tests @   
  get_tests @ 
  contains_tests @
  evaluate_tests @
  pretty_tests
;;

let arith_suite = "arithmetic_evaluation">:::all_arith_tests
;;

run_test_tt_main arith_suite
;;

let all_sexp_tests = [
    (* More tests here *)
  ]
;;

let sexp_suite = "sexp_parsing">:::all_sexp_tests
;;

run_test_tt_main sexp_suite
;;

