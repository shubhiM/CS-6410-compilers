open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Assembly
open Errors

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx";;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tvg name program expected = name>::test_run_valgrind program name expected;;

let tanf name program expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_aprogram;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

  (* integer overflow examples *)
  let prog_err_1  = "1073741824";;
  let prog_err_2  = "-1073741825";;
  let prog_err_20 = "1073741824 + 1";;
  let prog_err_21 = "-3 - -1073741825";;

  (* Unbound identifier examples *)
  let prog_err_3 = "x";;
  let prog_err_4 = "x + 1";;
  let prog_err_5 = "1 + x";;
  let prog_err_6 = "x * 1";;
  let prog_err_7 = "1 * x";;
  let prog_err_8 = "x - 1";;
  let prog_err_9 = "1 - x";;
  let prog_err_10 = "x < 1";;
  let prog_err_11 = "1 < x";;
  let prog_err_12 = "x > 1";;
  let prog_err_13 = "1 > x";;
  let prog_err_14 = "x <= 1";;
  let prog_err_15 = "1 <= x";;
  let prog_err_16 = "x >= 1";;
  let prog_err_17 = "1 >= x";;
  let prog_err_18 = "x == 1";;
  let prog_err_19 = "1 == x";;
  let prog_err_26 = "x && true";;
  let prog_err_27 = "true && x";;
  let prog_err_28 = "x || false";;
  let prog_err_29 = "false || x";;
  let prog_err_25 = "!(x)";;
  let prog_err_24 = "1 + add1(x)";;
  let prog_err_30 = "isbool(false) && isbool(x)";;
  (* Unbound function examples *)
  let prog_err_22 = "foo(x, y)";;
  let prog_err_31 = "isnum(1) && foo(true, false)";;
  let prog_err_35 = "1 + foo(true)";;
  let prog_err_32 = "isbool(1) && foo(true, false)";;
  let prog_err_33 = "print(1) && foo(true, false)";;
  let prog_err_36 = "foo(true, false) + 1";;

  (* Arity errors *)
  let prog_err_37 = "def foo(x, y):
                         (x + y)
                     foo()";;
  let prog_err_38 = "def foo(x, y):
                         (x + y)
                     foo(1, 2, 3)";;
  (* duplicate id *)
  let prog_err_39 = "let x = 1, y = 2, x = 3 in x + y";;
  let prog_err_41 = "let x = 1, y = 2, z = 3, y = 4 in x + y";;
  let prog_err_45 = "def foo(z, y, z):
                         (z * y)
                      1";;

  (* shadow errors *)
  let prog_err_43 = "let x = true, y = false in let x = y  in if y: true else: false";;
  let prog_err_44 = "let x = 1 in let y = x in let z = y in let y = z in x";;

  (* multiple errors *)
  let prog_err_47 = "def foo(x, y):
                        let x = x, y = y in let x = z in z
                     let foo = false in foo(true, false) && foo";;

  let prog_err_23 = "1 + foo(x)";;
  let prog_err_34 = "!(foo(1073741824))";;
  let prog_err_40 = "let x = 1, y = 2, x = 3 in x + y + z";;
  let prog_err_42 = "let x = 1, x = 2, x = 3 in x";;

  let prog_err_46 = "def foo(x , y):
                         let x = x, y = y in (x && y)
                     foo(true, false)";;

  let prog_err_48 = "let a = true, b = false in
                        if (let x = a, y = b in (let z = (x && y), w = (x || y) in let x = isbool(z) && isnum(w) in x)):
                          (a && b)
                          else:
                          (a || b)";;

  let prog_1 = "def foo(x, y):
                     (x + y)
                    foo(1, 2)";;

  let prog_2 = "def foo(x, y):
                     let z = x && !(y) in z
                let foo = false in foo(true, false) && foo";;

  let prog_3 = "def foo1(x, y):
                    (x + y)
                def foo2(z, w):
                    let x = z + w, y = z - w in foo1(x + y, y - x)
                (let x = 2, y = 3, foo1 = x, foo2 = y in foo1(x, y) + (foo1 * foo2)) +
                  (let x = 2, y = x * 3, foo1 = x, foo2 = y in foo2(x, y))";;

  let prog_5 = "def foo1(x, y):
                  x + y
                def foo2(x, y):
                 foo1(x, y)
                foo2(3, 4)"

  (* let prog_4 = "def iftest():
                    ifelse()
                def ifthen():
                    iftest()
                def ifelse():
                    ifthen()
                if iftest(): ifthen() else: ifelse()";; *)

  (* Recursive functions *)
  let prog_4 = "def factorial(x):
                   if x == 1: 1 else: x * factorial(x - 1)
                factorial(5)"

  let prog_6 = "def fib(n):
                   if n <= 1: 1 else: fib(n - 1) + fib(n - 2)
                fib(30)"

  let correct_programs = [
    t "prog_1" prog_1 "3";
    t "prog_2" prog_2 "false";
    t "prog_3" prog_3 "3";
    t "prog_4" prog_4 "120";
    t "prog_5" prog_5 "7";
    t "prog_6" prog_6 "1346269";
  ]


  let multiple_well_formedness_errs = [
    (* tested for all errors but including error message for only the first err *)
    te "t_err_23" prog_err_23 "The function name foo, used at ";
    te "t_err_34" prog_err_34 "The function name foo, used at ";
    te "t_err_40" prog_err_40 "The identifier x, redefined at ";
    te "t_err_42" prog_err_42 "The identifier x, redefined at ";
    te "t_err_47" prog_err_47 "The identifier x, defined at";

  ]

  let simple_well_formedness_err = [
     (* Overflow errors *)
     te "t_err_1" prog_err_1 "The number literal";
     te "t_err_2" prog_err_2 "The number literal";
     te "t_err_20" prog_err_20 "The number literal";
     te "t_err_21" prog_err_21 "The number literal";
     (* Ubound identifier errors*)
     te "t_err_3" prog_err_3 "The identifier x, used ";
     te "t_err_4" prog_err_4 "The identifier x, used ";
     te "t_err_5" prog_err_5 "The identifier x, used ";
     te "t_err_6" prog_err_6 "The identifier x, used ";
     te "t_err_7" prog_err_7 "The identifier x, used ";
     te "t_err_8" prog_err_8 "The identifier x, used ";
     te "t_err_9" prog_err_9 "The identifier x, used ";
     te "t_err_10" prog_err_10 "The identifier x, used ";
     te "t_err_11" prog_err_11 "The identifier x, used ";
     te "t_err_12" prog_err_12 "The identifier x, used ";
     te "t_err_13" prog_err_13 "The identifier x, used ";
     te "t_err_14" prog_err_14 "The identifier x, used ";
     te "t_err_15" prog_err_15 "The identifier x, used ";
     te "t_err_16" prog_err_16 "The identifier x, used ";
     te "t_err_17" prog_err_17 "The identifier x, used ";
     te "t_err_18" prog_err_18 "The identifier x, used ";
     te "t_err_19" prog_err_19 "The identifier x, used ";
     te "t_err_26" prog_err_26 "The identifier x, used ";
     te "t_err_27" prog_err_27 "The identifier x, used ";
     te "t_err_28" prog_err_28 "The identifier x, used ";
     te "t_err_29" prog_err_29 "The identifier x, used ";
     te "t_err_24" prog_err_24 "The identifier x, used ";
     te "t_err_25" prog_err_25 "The identifier x, used ";
     te "t_err_30" prog_err_30 "The identifier x, used ";
    (* Unbound function name error *)
     te "t_err_22" prog_err_22 "The function name foo, used at ";
     te "t_err_31" prog_err_31 "The function name foo, used at ";
     te "t_err_32" prog_err_32 "The function name foo, used at ";
     te "t_err_33" prog_err_33 "The function name foo, used at ";
     te "t_err_35" prog_err_35 "The function name foo, used at ";
     te "t_err_36" prog_err_36 "The function name foo, used at ";

    (* Arity errors *)
     te "t_err_37" prog_err_37 "The function called at";
     te "t_err_38" prog_err_38 "The function called at";


     (* duplicate id error *)
     te "t_err_37" prog_err_39 "The identifier x, redefined at ";
     te "t_err_38" prog_err_41 "The identifier y, redefined at ";
     te "t_err_45" prog_err_45 "The identifier z, redefined at ";

     (* shadow error *)
     te "t_err_43" prog_err_43 "The identifier x, defined at ";
     te "t_err_44" prog_err_44 "The identifier y, defined at ";
     te "prog_err_48" prog_err_48 "The identifier x, defined at";
   ]


   (* tests from homework 4 to test the standard non function body expressions *)
   let int_unary_op_tests = [
    t "t_int_unary_1" "add1(5)" "6"
    ; t "t_int_unary_2" "sub1(5)" "4"
    ; t "t_int_unary_3" "add1(-5)" "-4"
    ; t "t_int_unary_4" "sub1(-5)" "-6"
    ; t "t_int_unary_5" "sub1(sub1(5))" "3"
    ; t "t_int_unary_6" "add1(sub1(5))" "5"
    ; t "t_int_unary_7" "sub1(add1(sub1(5)))" "4"
  ]


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
    ; t "t_int_binary_10" "add1(1) + sub1(-1)" "0"
    ; t "t_int_binary_11" "add1(sub1(5)) * sub1(add1(sub1(5)))" "20"
    ; t "t_int_binary_12" "3 + sub1(3) * add1(2)" "15"
    ; t "t_int_binary_13" "3 + sub1(3) * add1(2)" "15"
    ; t "t_int_binary_14" "sub1(1 * 2 + 3)" "4"
    ; t "t_int_binary_15" "sub1(1 * add1(2) + 3)" "5"
  ]
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
  ]

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
  ]

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

    t "let_17" {| let x = (let y = 2 in y) in x |} "2";
    t "let_18" {| let x = 2 * 2 + 1, y = x * 2, z = y - (x * y) in z |} "-40";
    t "let_19" {| let x = add1(4), y = x * add1(1), z = y - (x * y) in z |} "-40";
  ]

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

    t "if_16" {| let c1 = 1 in
                  let c2 = 2 in
                    let x = (if c1 == 1: (5 + 5) else: (6 * 2)) in
                      let y = (if c2 == 2: (x * 3) else: (x + 5)) in
                        x + y |} "40";
    t "if_17" {| if (let x = (1 < 2) in x && x): 1 else: 2 |} "1";
    t "if_18" {| if true: (let x = 0 in x + 1) else: 2 |} "1";
    t "if_19" {| if false: 0 else: (let x = 0 in x + 1) |} "1";
    t "if_20" {| if (let x = false in let y = true in x || y): 1 else: 2 |} "1";
    t "if_21" {| if true: (let x = false in let y = true in x || y) else: 2 |} "true";
    t "if_22" {| if (let x = true in x): (let x = 2 in x) else: 0 |} "2";
    t "if_23" {| if ((let x = 2 in x) - 2) > 0: 0
                 else: (if ((let y = 2 in y * 2) - 4) > 0: 0 else: 10) |} "10";
    t "if_24" {| (if ((let x = 2 in x) - 2) < 0: 0
                  else: (if (let y = false in y): true else: false)) && true |} "false";
    t "if_25" {| if (let x = false, y = true in x || y): (let x = 1, y = 2 in x * y) else: 1 |} "2"
  ]

  let arith_err_tests = [
    te "t_arith_err_1" "add1(true)" "arithmetic expected a number"
    ; te "t_arith_err_2" "add1(false)" "arithmetic expected a number"
    ; te "t_arith_err_3" "sub1(true)" "arithmetic expected a number"
    ; te "t_arith_err_4" "sub1(false)" "arithmetic expected a number"
    ; te "t_arith_err_5" "sub1(true)" "arithmetic expected a number"
    ; te "t_arith_err_6" "add1(sub1(false))" "arithmetic expected a number"
    ; te "t_arith_err_7" "true * false" "arithmetic expected a number"
    ; te "t_arith_err_8" "true * 1" "arithmetic expected a number"
    ; te "t_arith_err_9" "1 * true" "arithmetic expected a number"
    ; te "t_arith_err_10" "true + false" "arithmetic expected a number"
    ; te "t_arith_err_11" "1 + false" "arithmetic expected a number"
    ; te "t_arith_err_12" "true + 1" "arithmetic expected a number"
    ; te "t_arith_err_13" "true - false" "arithmetic expected a number"
    ; te "t_arith_err_14" "1 - false" "arithmetic expected a number"
    ; te "t_arith_err_15" "true - 1" "arithmetic expected a number"
    ; te "t_arith_err_16" "1 + true" "arithmetic expected a number"
    ; te "t_arith_err_17" "1 + 2 * 3 + 4 - 5 * true" "arithmetic expected a number"
  ]

  let logical_err_tests = [
      te "t_int_log_err_1" "!(1)" "logic expected a boolean"
    ; te "t_int_log_err_2" "1 && 2" "logic expected a boolean"
    ; te "t_int_log_err_3" "1 || 2" "logic expected a boolean"
    ; te "t_int_log_err_4" "1 && true" "logic expected a boolean"
    ; te "t_int_log_err_7" "2 || true" "logic expected a boolean"
    (* Could have been short circuited as design choice *)
    ; te "t_int_log_err_5" "false && (1 + 2 + 3)" "logic expected a boolean"
    ; te "t_int_log_err_6" "true || 2" "logic expected a boolean"
  ]

  let cmp_err_tests = [
     te "t_cmp_err_1" "1 < true" "comparison expected a number"
    ; te "t_cmp_err_2" "true > false" "comparison expected a number"
    ; te "t_cmp_err_3" "false == false" "comparison expected a number"
    ; te "t_cmp_err_4" "1 <= true" "comparison expected a number"
    ; te "t_cmp_err_5" "false >= 2" "comparison expected a number"
    ; te "t_cmp_err_6" "false >= false" "comparison expected a number"
    ; te "t_cmp_err_7" "add1(1) >= true" "comparison expected a number"
    ; te "t_cmp_err_8" "add1(true) >= false" "comparison expected a number"
    ; te "t_cmp_err_9" "(1 + 1 == 1 - 1)" "comparison expected a number"
    ; te "t_cmp_err_10" "1 >= 2 && 1-1 == 0" "comparison expected a number"
  ]

  let let_and_if_err = [
    te "if_err_1" {| if 1 < 3 < 4: (4 - 3) * (1 + 2) else: 0 |} "comparison expected a number";
    te "if_err_2" {| if (let x = 1 in x) < (let y = 2 in y) < (let z = 3 in z): true else: false |} "comparison expected a number";
    te "if_err_3" {| if (let x = 1 in x) < (let x = 2 in x) < (let x = 3 in x): true else: false |} "comparison expected a number";
    te "let_err_1" {| let x = 1, y = 2, z = if x: 3 else: 4 in z |} "if expected a boolean";
  ]

  let suite =
  "suite">:::
    simple_well_formedness_err
  @ multiple_well_formedness_errs
  (* @ int_unary_op_tests
  @ int_binary_op_tests
  @ bool_tests
  @ int_cmp_op_tests
  @ if_tests
  @ let_tests
  @ arith_err_tests
  @ logical_err_tests
  @ let_and_if_err
  @ correct_programs *)


let () =
  run_test_tt_main suite
;;
