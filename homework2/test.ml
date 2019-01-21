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
   t "let_0" "(let ((x 10)) x)" "10";
   t "let_1" "(let ((x 2)) (add1 x))" "3";
   t "let_2" "(let ((x (add1 2))) (add1 x))" "4";
   t "let_3" "(let ((x 3) (y 4)) (add1 x))" "4";
   t "let_4" "(let ((x 3) (y 4)) (add1 y))" "5";
   t "let_5" "(let ((x 3) (y 4) (z 10) (w 8)) w)" "8";
   t "let_6" "(let ((x 2)) (let ((y (add1 x))) (add1 y)))" "4";
   t "let_7" "(let ((x 2)) (let ((x (add1 x))) x))" "3"; 
   
   (* top level bindings are accessible in the nested lets *)
   t "let_8" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (y (add1 z) ) ) x ))) " "1"; 
   
   (* lexical scope is maintained *)
   t "let_9" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (x (add1 z) ) ) x ))) " "4"; 
   
   t "let_10" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (y (add1 z) ) ) z ))) " "3"; 

   te "x_err" "x" "Unbound identifier";
   te "let_err_1" "(let ((x (add1 x))) (add1 x))" "Unbound identifier";
   te "let_err_4" "(let ((x (add1 y))) (add1 x))" "Unbound identifier";

  ]
;;


let () =
  run_test_tt_main suite
;;
