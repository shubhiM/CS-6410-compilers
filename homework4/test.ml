open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;
let te name program expected = name>::test_err program name expected;;
(*
let tprog filename expected = filename >:: test_run_input filename expected
let teprog filename expected = filename >:: test_err_input filename expected
*)

let forty = "let x = 40 in x"
let fals = "let x = false in x"
let tru = "let x = true in x"

let int_tests =
  [
  (* Integer bounds *)
    t "t_int_upper_bound" "1073741823" "1073741823"
  ; t "t_int_lower_bound" "-1073741824" "-1073741824"

  (* Arithmetic unary operators *)

  ; t "t_int_unary_1" "add1(5)" "6"
  ; t "t_int_unary_2" "sub1(5)" "4"
  ; t "t_int_unary_3" "add1(-5)" "-4"
  ; t "t_int_unary_4" "sub1(-5)" "-6"
  ; t "t_int_unary_5" "(sub1 (sub1 5))" "3"
  ; t "t_int_unary_6" "(add1 (sub1 5))" "5"
  ; t "t_int_unary_7" "(sub1 (add1 (sub1 5)))" "4"

  (* Arithmetic binary operators *)
  ; t "t_int_binary_1" "1 * 2 + 3" "5"
  ; t "t_int_binary_2" "1 * (2 + 3)" "6"
  ; t "t_int_binary_3" "(1 * 2) + 3" "5"
  ; t "t_int_binary_4" "1 + 2 * 3 - 4" "5"
  ; t "t_int_binary_6" "((1 + (2 * 3)) - 4)" "3"
  ; t "t_int_binary_7" "1 + (2 * 3) - 4" "3"
  ; t "t_int_binary_8" "1 + 2 + 3 + 4 + 5 + 6" "21"
  ; t "t_int_binary_9" "1 * 2 * 3 * 4 * 5" "120"
  ; t "t_int_binary_10" "(add1(1)) + (sub1(1))" "0"
  ; t "t_int_binary_11" "(add1 (sub1 5)) * (sub1 (add1 (sub1 5)))" "20"
  ; t "t_int_binary_12" "3 + sub1(3) * add1(2)" "9"
  ; t "t_int_binary_13" "3 + sub1(3) * add1(2)" "9"
  ; t "t_int_binary_14" "sub1(1 * 2 + 3)" "4"
  ; t "t_int_binary_15" "sub1(1 * add1(2) + 3)" "5"

  (* Comparison expressions *)
  ; t "t_int_cmp_1" "1 < 2" "true"
  ; t "t_int_cmp_2" "1 > 2" "false"
  ; t "t_int_cmp_3" "1 == 2" "false"
  ; t "t_int_cmp_4" "1 <= 2" "true"
  ; t "t_int_cmp_5" "1 >= 2" "false"
  ; t "t_int_cmp_6" "2 >= 2" "true"
  ; t "t_int_cmp_7" "add1(1) >= 2" "true"
  ; t "t_int_cmp_8" "add1(1) >= sub1(2)" "false"
  ; t "t_int_cmp_9" "(1 + 2) >= (3 * 1)" "true"
  ; t "t_int_cmp_10" "1 * 2 * 3 * 4 * 5 == 120" "true"
  ; t "t_int_cmp_11" "(1 * 2 * 3 * 4 * 5) == 120" "true"


  (* Illegal arithmetic expressions *)
  ; te "t_arith_err_1" "add1(true)" "arithmetic expected a number"
  ; te "t_arith_err_2" "add1(false)" "arithmetic expected a number"
  ; te "t_arith_err_3" "sub1(true)" "arithmetic expected a number"
  ; te "t_arith_err_4" "sub1(false)" "arithmetic expected a number"
  ; te "t_arith_err_5" "sub1(true)" "arithmetic expected a number"
  ; te "t_arith_err_6" "add1(sub1(false))" "arithmetic expected a numbe1r"
  ; te "t_arith_err_7" "true * false" "arithmetic expected a number"
  ; te "t_arith_err_8" "true + false" "arithmetic expected a number"
  ; te "t_arith_err_9" "false - true" "arithmetic expected a number"
  ; te "t_arith_err_10" "true + 1" "arithmetic expected a number"
  ; te "t_arith_err_11" "1 + 2 * 3 + 4 - 5 * true" "arithmetic expected a number"

  (* Illegal Logical expressions *)

  (*
  ; t "t_int_log_err_1" "not(1)" "expected a boolean"
  ; t "t_int_log_err_1" "1 > 2" "false"
  ; t "t_int_log_err_1" "1 == 2" "false"
  ; t "t_int_cmp_4" "1 <= 2" "true"
  ; t "t_int_cmp_5" "1 >= 2" "false"
  ; t "t_int_cmp_6" "2 >= 2" "true"
  ; t "t_int_cmp_7" "add1(1) >= 2" "true"
  ; t "t_int_cmp_8" "add1(1) >= sub1(2)" "false"
  ; t "t_int_cmp_9" "(1 + 2) >= (3 * 1)" "true"
  ; t "t_int_cmp_10" "1 * 2 * 3 * 4 * 5 == 120" "true"
  ; t "t_int_cmp_11" "(1 * 2 * 3 * 4 * 5) == 120" "true"
*)

  (* Illegal comparison expressions *)
  ; te "t_cmp_err_1" "1 < true" "comparison expected a boolean"
  ; te "t_cmp_err_2" "true > false" "comparison expected a boolean"
  ; te "t_cmp_err_3" "false == false" "comparison expected a boolean"
  ; te "t_cmp_err_4" "1 <= true" "comparison expected a boolean"
  ; te "t_cmp_err_5" "false >= 2" "comparison expected a boolean"
  ; te "t_cmp_err_6" "false >= false" "comparison expected a boolean"
  ; te "t_cmp_err_7" "add1(1) >= true" "comparison expected a boolean"
  ; te "t_cmp_err_8" "add1(true) >= false" "arithmetic expected a number"




  (* Integer overflow and underflow tests *)
  ; te "t_int_overflow_1" "add1(add1(1073741822))" "Compile-time integer overflow"
  ; te "t_int_overflow_2" "2147483647" "Compile-time integer overflow"
  ; te "t_int_underflow_1" "sub1(sub1(sub1(-1073741822))" "Compile-time integer underflow"
  ; te "t_int_underflow_2" "-2147483648" "Compile-time integer underflow"

  ];;

(* true/false/let/if/else -> these are keywords *)
(* true/false is also a value *)
(* anything else is an identifier and need to run through the scoping rules *)


let bool_tests  = [
];;

let let_tests = [
    t "t_1" "if 5: 4 else: 2" "4";
    t "t_2" "if 0: 4 else: 2" "2";
    t "t_3" "1" "1";
    te "t_4" "foo" "Name foo not found";
    t "t_5" "add1(10)" "11";
    t "t_6" "sub1(0)" "-1";
    te "t_7" "add1(y)" "Name y not found";
    t "t_8" "-3 + 2" "-1";
    t "t_9" "3 + 2" "5";
    t "t_10" "3 + -2" "1";
    t "t_11" "-3 + -2" "-5";
    t "t_12" "0 * 5 " "0";
    t "t_13" "1 * 5"  "5";
    te "t_14" "x * y" "Name x not found";
    te "t_15" "1 * y" "Name y not found";
    t "t_16" "1 - 1" "0";
    t "t_17" "1 - -10" "11";
    t "t_18" "if 5: add1(4) else: 2" "5";
    t "t_19" "if 0: 4 else: sub1(2)" "1";
    t "t_20" "let x=2 in add1(x)" "3";
    t "t_21" "let x=2 in let y = add1(x) in sub1(y)" "2";
];;

let suite =
"suite">:::
 [
  t "forty" "add1(40)" "41"
  (*t "fals" fals "false";
  t "tru" tru "true";*)
 ];;


let () =
  run_test_tt_main suite
;;
