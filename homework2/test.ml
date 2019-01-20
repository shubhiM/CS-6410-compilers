open Compile
open Runner
open Printf
open OUnit2

let t (name : string) (program : string) (expected : string) : OUnit2.test =
  name>::test_run program name expected;;
let te (name : string) (program : string) (expected_err : string) : OUnit2.test =
  name>::test_err program name expected_err;;

let suite : OUnit2.test =
"suite">:::
 [
   t "forty_one" "41" "41";
   t "add_1_const" "(add1 2)" "3";
   t "sub_1_const" "(sub1 2)" "1";
   te "x_not_in_env" "x" "Unbound identifie";
 (*  te "nyi" "(let ((x 10)) x)" "not yet implemented" *)


  ]
;;


let () =
  run_test_tt_main suite
;;
