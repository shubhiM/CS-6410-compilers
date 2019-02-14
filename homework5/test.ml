open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx";;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tvg name program expected = name>::test_run_valgrind program name expected;;

(* let tw name program expected = name>::test_run_wellformed program name expected;; *)

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
let prog_err_24 = "1 + add1(x)";;
let prog_err_25 = "!(x)";;
let prog_err_30 = "isbool(false) && isbool(x)";;
let prog_err_23 = "1 + foo(x)";;

(* Unbound function examples *)
let prog_err_22 = "foo(x, y)";;
let prog_err_31 = "isnum(1) && foo(true, false)";;
let prog_err_35 = "1 + foo(true)";;
let prog_err_32 = "isbool(1) && foo(true, false)";;
let prog_err_33 = "print(1) && foo(true, false)";;
let prog_err_34 = "!(foo(1073741824))";;
let prog_err_36 = "foo(true, false) + 1";;


let well_formedness_err = [

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
   te "t_err_23" prog_err_23 "The identifier x, used ";
   te "t_err_30" prog_err_30 "The identifier x, used ";

  (* Unbound function name error *)
   te "t_err_22" prog_err_22 "The function name foo, used at ";
   te "t_err_31" prog_err_31 "The function name foo, used at ";
   te "t_err_32" prog_err_32 "The function name foo, used at ";
   te "t_err_33" prog_err_33 "The function name foo, used at ";
   te "t_err_34" prog_err_34 "The function name foo, used at ";
   te "t_err_35" prog_err_35 "The function name foo, used at ";
   te "t_err_36" prog_err_36 "The function name foo, used at ";

 ]



(* let well_formedness_ok = []; *)

let suite =
"suite">:::well_formedness_err

let () =
  run_test_tt_main suite
;;
