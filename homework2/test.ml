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
   
   (* lexical scope is maintained *)
   t "let_8" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (y (add1 z) ) ) x ))) " "1"; 
   
   t "let_9" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (x (add1 z) ) ) x ))) " "4"; 
   
   t "let_10" "(let ( (x 1) (y 2) )
                (let ( (z (add1 y)) )
                  (let ( (y (add1 z) ) ) z ))) " "3";

   t "let_11" "(let ((x (let ((y (add1 2))) (sub1 y)))) x)" "2";
   t "let_12" "(let ((x 5) (y x)) y)" "5";
   t "let_13" "(let ((x 5) (y (sub1 x))) (sub1 y))" "3";
   t "let_14" "(let ((x 5) (y (sub1 x)) (z (sub1 y))) (sub1 z))" "2";
   t "let_15" "(let ((x 5) (y 6) (z y)) z)" "6";
   t "let_16" "(let ((x (let ((x 5)) (sub1 x)))) x)" "4";

   (* nested let named expressions *)
   t "let_17" "(let ((z (let ( (x 1) (y 2) )
                           (let ( (z (add1 y)) )
                              (let ( (z (add1 z) ) ) z ))))) z) " "4";

  t "let_18" 
      "(let ((z 1))
        (let ( (z (let ((y 2)) 
                     (let ((z (add1 y)))
                        (let ((z (add1 z))) z ))))) z))" "4";

   te "x_err" "x" "Unbound identifier";
   te "let_err_1" "(let ((x (add1 x))) (add1 x))" "Unbound identifier";
   te "let_err_4" "(let ((x (add1 y))) (add1 x))" "Unbound identifier";
  
    ]
;;


let () =
  run_test_tt_main suite
;;
