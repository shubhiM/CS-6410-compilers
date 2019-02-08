open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;
let te name program expected = name>::test_err program name expected;;

let int_bound_tests = [
  t "t_int_upper_bound" "1073741823" "1073741823"
  ; t "t_int_lower_bound" "-1073741824" "-1073741824"
];;

let int_range_err = [
  (* Integer overflow and underflow tests *)
  te "t_int_overflow_1" "add1(add1(1073741822))" "Compile-time integer overflow"
  ; te "t_int_overflow_2" "2147483647" "Compile-time integer overflow"
  ; te "t_int_underflow_1" "sub1(sub1(sub1(-1073741822))" "Compile-time integer underflow"
  ; te "t_int_underflow_2" "-2147483648" "Compile-time integer underflow"
];;

let int_unary_op_tests = [
  t "t_int_unary_1" "add1(5)" "6"
  ; t "t_int_unary_2" "sub1(5)" "4"
  ; t "t_int_unary_3" "add1(-5)" "-4"
  ; t "t_int_unary_4" "sub1(-5)" "-6"
  ; t "t_int_unary_5" "sub1(sub1(5))" "3"
  ; t "t_int_unary_6" "add1(sub1(5))" "5"
  ; t "t_int_unary_7" "(sub1 (add1 (sub1 (5))))" "4"
];;

(* order of evaluation is left to right that obeys paranthesis *)
let int_binary_op_tests = [
   t "t_int_binary_1" "1 * 2 + 3" "5"
  ; t "t_int_binary_2" "1 * (2 + 3)" "5"
  ; t "t_int_binary_3" "(1 * 2) + 3" "5"
  ; t "t_int_binary_4" "1 + 2 * 3 - 4" "5"
  ; t "t_int_binary_6" "((1 + (2 * 3)) - 4)" "3"
  ; t "t_int_binary_7" "1 + (2 * 3) - 4" "3"
  ; t "t_int_binary_8" "1 + 2 + 3 + 4 + 5 + 6" "21"
  ; t "t_int_binary_9" "1 * 2 * 3 * 4 * 5" "120"
  ; t "t_int_binary_10" "(add1(1)) + (sub1(-1))" "0"
  ; t "t_int_binary_11" "(add1 (sub1(5))) * (sub1 (add1 (sub1 (5))))" "20"
  ; t "t_int_binary_12" "3 + sub1(3) * add1(2)" "15"
  ; t "t_int_binary_13" "3 + sub1(3) * add1(2)" "15"
  ; t "t_int_binary_14" "sub1(1 * 2 + 3)" "4"
  ; t "t_int_binary_15" "sub1(1 * add1(2) + 3)" "5"
];;

let int_cmp_op_tests = [
  t "t_int_cmp_1" "1 < 2" "true"
  ; t "t_int_cmp_2" "1 > 2" "false"
  ; t "t_int_cmp_3" "1 == 2" "false"
  ; t "t_int_cmp_4" "1 <= 2" "true"
  ; t "t_int_cmp_5" "1 >= 2" "false"
  ; t "t_int_cmp_6" "2 >= 2" "true"
  ; t "t_int_cmp_7" "add1(1) >= 2" "true"
  ; t "t_int_cmp_8" "add1(1) >= sub1(2)" "true"
  ; t "t_int_cmp_9" "(1 + 2) >= (3 * 1)" "true"
  ; t "t_int_cmp_10" "1 * 2 * 3 * 4 * 5 == 120" "true"
  ; t "t_int_cmp_11" "(1 * 2 * 3 * 4 * 5) == 120" "true"
];;

let bool_tests  = [
  t "t_bool_1" "true" "true";
  t "t_bool_2" "false" "false";
  t "t_bool_3" "true && true" "true";
  t "t_bool_4" "true && false" "false";
  t "t_bool_5" "false && false" "false";
  t "t_bool_6" "false && true" "false";
  t "t_bool_7" "true || true" "true";
  t "t_bool_8" "true || false" "true";
  t "t_bool_9" "false || false" "false";
  t "t_bool_10" "false || true" "true";
  t "t_bool_11" "!(false)" "true";
  t "t_bool_12" "!(true)" "false";
  t "t_bool_13" "isbool(true)" "true";
  t "t_bool_14" "isbool(false)" "true";
  t "t_bool_15" "isbool(1)" "false";
  t "t_bool_16" "isbool(0)" "false";
  t "t_bool_17" "isbool(-1)" "false";
  t "t_bool_18" "isnum(false)" "false";
  t "t_bool_19" "isnum(true)" "false";
  t "t_bool_20" "isnum(1)" "true";
  t "t_bool_21" "isnum(0)" "true";
  t "t_bool_22" "isnum(-1)" "true"
];;

let let_tests = [
  t "let_1" "let x = 1 in x" "1";
  t "let_2" "let x = 2, y = 3 in y" "3";
  t "let_3" {| let x = 2 in
                 let y = x in y |}
  "2";
  t "let_4" {| let x = 2 in
                 let y = 3 in x |}
  "2";
  t "let_5" {| let x = 2, y = 3 in
                 let z = 4 in x |}
  "2";
  t "let_6" {| let x = 2 in x + 3 |}
  "5";
  t "let_7" {| let x = 2, y = 3 in
                   x * y + x |}
  "8";
  t "let_8" {| let x = 2, y = 3 in
                   let z = 4 in
                       (x + z) * (y - z) |}
  "-6";
  t "let_9" {| let x = 2, y = 3 in
                       sub1(x) * add1(y) |}
  "4";
  t "let_10" {| let x = 2 in
                  let y = 3 in
                      sub1(x) * add1(y) |}
  "4";
  t "let_11" {| let x = 2 in
                  let y = 3 in
                      x + sub1(x) * add1(y) |}
  "12";
  t "let_12" {| let x = 2 * (3 + 2) in
                  let y = sub1(x) in
                      x + 3 * y |}
  "117";
  t "let_13" {| let x = 2 * (3 + 2) in
                  let y = sub1(x) * 2 in
                      x + 3 * y |}
  "234";
  t "let_14" {| let x = 1 in
                  let y = if ((x - 1) > 0) : 0 else: x in x |} "1";
  t "let_15" {| let x = 2 in
                  let y = if ((x - 1) == 1): x else: 0 in x |} "2";
  t "let_16" {| let x = 1, y = 2, z = if x: 3 else: 4 in z |} "3";
  t "let_17" {| let x = (let y = 2 in y) in x |} "2";
  t "let_18" {| let x = 2 * 2 + 1, y = x * 2, z = y - (x * y) in z |} "-40";
  t "let_19" {| let x = add1(4), y = x * add1(1), z = y - (x * y) in z |} "-40";
];;

let if_tests = [
  t "if_1" "if true: 4 else: 2" "4";
  t "if_2" "if false: 4 else: 2" "2";
  t "if_3" "if (1 > 2): 4 else: 2" "2";
  t "if_4" "if (1 >= 2): 4 else: 2 * (3 - 2)" "2";
  t "if_5" "if ((1 + 1) == (1 - 1)): 4 else: 2" "2";
  t "if_6" "if (1 >= 2) || (1 - 1 == 0): 4 else: 2" "4";
  t "if_7" "if (1 >= 2) && (1 - 1 == 0): 4 else: 2" "2";
  t "if_8" "if true: true else: false" "true";
  t "if_9" "if !(true): true else: false" "false";
  t "if_10" {| let x = 2, y = 2 in
                  if y - x == 0: 4 else: 2 |} "4";
  t "if_11" {| let x = 2, y = 2 in
                  if y * x == 0: 0
                  else: y * x |} "4";
  t "if_12" {| let x = if true: 2 else: 3 in
                  x |} "2";
  t "if_13" {| let x = true, y = 3 in
                  if x: y else: x |} "3";
  t "if_14" {| let x = false, y = if x: x else: 2 in
                  y |} "2";
  t "if_15" {| let x = 0, y = if x>0: x else: 2 in
                  y |} "2";

  (* TODO: Getting signal 10 *)
  (* t "if_16" {| let c1 = 1 in
                let c2 = 2 in
                  let x = (if c1 == 1: (5 + 5) else: (6 * 2)) in
                    let y = (if c2 == 2: (x * 3) else: (x + 5)) in
                      x + y |} "40"; *)

  t "if_17" {| if (let x = (1 < 2) in x && x): 1 else: 2 |} "1";
  t "if_18" {| if true: (let x = 0 in x + 1) else: 2 |} "1";
  t "if_19" {| if false: 0 else: (let x = 0 in x + 1) |} "1";

  (* TODO: Getting signal 10 *)
  (* t "if_20" {| if (let x = false, y = true in x || y): (let x = 1, y = 2 in x * y) else: 1 |} "2" *)


  t "if_20" {| if (let x = false in let y = true in x || y): 1 else: 2 |} "1";
  t "if_21" {| if true: (let x = false in let y = true in x || y) else: 2 |} "true";
  t "if_22" {| if (let x = true in x): (let x = 2 in x) else: 0 |} "2";
  t "if_23" {| if 1 < 3 < 4: (4 - 3) * (1 + 2) else: 0 |} "3";

  (* TODO: signaling -10 *)
  (* t "if_24" {| if ((let x = 2 in x) - 2) > 0: 0
               else: (if ((let y = 2 in y * 2) - 4) > 0: 0 else: 10) |} "10"; *)

  t "if_25" {| (if ((let x = 2 in x) - 2) < 0: 0
                else: (if (let y = false in y): true else: false)) && true |} "false";

  t "if_26" {| if (let x = 1 in x) < (let y = 2 in y) < (let z = 3 in z): true else: false |} "true";

 (* TODO: why is this evaluating to false *)
  (* t "if_27" {| if (let x = 1 in x) < (let x = 2 in y) < (let x = 3 in z): true else: false |} "true"; *)

];;

let arith_err_tests = [
  te "t_arith_err_1" "add1(true)" "Expected number"
  ; te "t_arith_err_2" "add1(false)" "Expected number"
  ; te "t_arith_err_3" "sub1(true)" "Expected number"
  ; te "t_arith_err_4" "sub1(false)" "Expected number"
  ; te "t_arith_err_5" "sub1(true)" "Expected number"
  ; te "t_arith_err_6" "add1(sub1(false))" "Expected number"

  ; te "t_arith_err_7" "true * false" "Expected number"
  ; te "t_arith_err_8" "true * 1" "Expected number"
  ; te "t_arith_err_9" "1 * true" "Expected number"

  ; te "t_arith_err_10" "true + false" "Expected number"
  ; te "t_arith_err_11" "1 + false" "Expected number"
  ; te "t_arith_err_12" "true + 1" "Expected number"

  ; te "t_arith_err_13" "true - false" "Expected number"
  ; te "t_arith_err_14" "1 - false" "Expected number"
  ; te "t_arith_err_15" "true - 1" "Expected number"

  ; te "t_arith_err_16" "1 + true" "Expected number"
  ; te "t_arith_err_17" "1 + 2 * 3 + 4 - 5 * true" "Expected number"
];;

let logical_err_tests = [
    te "t_int_log_err_1" "!(1)" "Expected boolean"
  ; te "t_int_log_err_2" "1 && 2" "Expected boolean"
  ; te "t_int_log_err_3" "1 || 2" "Expected boolean"
  ; te "t_int_log_err_4" "1 && true" "Expected boolean"
  ; te "t_int_log_err_7" "2 || true" "Expected boolean"
  (* Could have been short circuited as design choice *)
  ; te "t_int_log_err_5" "false && (1 + 2 + 3)" "Expected boolean"
  ; te "t_int_log_err_6" "true || 2" "Expected boolean"
];;

let cmp_err_tests = [
   te "t_cmp_err_1" "1 < true" "comparison expected a boolean"
  ; te "t_cmp_err_2" "true > false" "comparison expected a boolean"
  ; te "t_cmp_err_3" "false == false" "comparison expected a boolean"
  ; te "t_cmp_err_4" "1 <= true" "comparison expected a boolean"
  ; te "t_cmp_err_5" "false >= 2" "comparison expected a boolean"
  ; te "t_cmp_err_6" "false >= false" "comparison expected a boolean"
  ; te "t_cmp_err_7" "add1(1) >= true" "comparison expected a boolean"
  ; te "t_cmp_err_8" "add1(true) >= false" "comparison expected a boolean"
  ; te "t_cmp_err_9" "(1 + 1 == 1 - 1)" "comparison expected a boolean"
  ; te "t_cmp_err_10" "1 >= 2 && 1-1 == 0" "comparison expected a boolean"
];;


(* true/false/let/if/else -> these are keywords *)
(* true/false is also a value *)
(* anything else is an identifier and need to run through the scoping rules *)


let suite =
"suite">:::
int_bound_tests
@ int_unary_op_tests
@ int_binary_op_tests
@ bool_tests
@ int_cmp_op_tests
@ if_tests
@ let_tests
@ arith_err_tests
@ logical_err_tests
;;

let () =
  run_test_tt_main suite
;;
