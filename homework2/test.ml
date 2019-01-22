open Compile
open Runner
open Printf
open OUnit2

let t (name : string) (program : string) (expected : string) : OUnit2.test =
  name >:: test_run program name expected

let te (name : string) (program : string) (expected_err : string) : OUnit2.test
    =
  name >:: test_err program name expected_err

let int_tests =
  [ t "int_1" "41" "41"
  ; t "int_2" "2147483647" "2147483647"
  ; t "int_3" "(add1 2147483647)" "-2147483648"
  ; t "add1_5" "(add1 5)" "6"
  ; t "sub1_5" "(sub1 5)" "4"
  ; t "add1_neg5" "(add1 -5)" "-4"
  ; t "sub1_neg5" "(sub1 -5)" "-6"
  ; t "sub1_sub1" "(sub1 (sub1 5))" "3"
  ; t "add1_sub1" "(add1 (sub1 5))" "5"
  ; t "sub1_add1_sub1" "(sub1 (add1 (sub1 5)))" "4" ]

let more_tests =
  [ t "more_0"
      {| (let ((x 1))
          (let ((y (sub1 x)))
               x))
  |} "1"
  ; t "more_1"
      {| (let ((x 1) (y (add1 x)))
  	      (let ((z (sub1 y)))
               (add1 (sub1 y)))) 
  |}
      "2"
  ; t "more_2"
      {| (let ((x 1) (y (let ((x 10)) (sub1 (sub1 x)))))
  	      (let ((x y) (z x))
  	           (sub1 z))) 
  |}
      "7"
  ; t "more_3" {| (let ((x 1) (y 2) (z 0)) 
  	      z)
  |} "0"
  ; t "more_4"
      {| (let ((x 1))
  	      (let ((x 2))
  	      	   (let ((x 3)) x)))
  |}
      "3"
  ; t "more_5" {| (let ((x 1))
  	      (let ((y 2)) x)) 
  |} "1"
  ; t "more_6"
      {| (let ((a 1))
  	      (let ((x 2) (a (add1 x)))
  	           (let ((x 3)) a)))
  |}
      "3"
  ; t "more_7"
      {| (let ((a 1))
          (let ((b (sub1 a)) (c (sub1 (sub1 b))) (d c))
               (let ((e (let ((f c)) (sub1 f))))
                    (sub1 (let ((g (sub1 e))) g)))))
  |}
      "-5"
  ; t "more_8"
      {| (let ((x 1))
          (let ((x (add1 x)))
               (let ((x (add1 x))) 
                    (let ((x (add1 x))) x))))
  |}
      "4" ]

let let_tests =
  [ t "let_0" "(let ((x 10)) x)" "10"
  ; t "let_1" "(let ((x 2)) (add1 x))" "3"
  ; t "let_2" "(let ((x (add1 2))) (add1 x))" "4"
  ; t "let_3" "(let ((x 3) (y 4)) (add1 x))" "4"
  ; t "let_4" "(let ((x 3) (y 4)) (add1 y))" "5"
  ; t "let_5" "(let ((x 3) (y 4) (z 10) (w 8)) w)" "8"
  ; t "let_6" "(let ((x 2)) (let ((y (add1 x))) (add1 y)))" "4"
  ; t "let_7" "(let ((x 2)) (let ((x (add1 x))) x))" "3"
  ; t "let_8" "(let ( (x (add1 2) ) ) (add1 (sub1 x)))" "3"
    (* lexical scope tests *)
  ; t "let_9"
      "(let ( (x 1) (y 2) )\n\
      \                (let ( (z (add1 y)) )\n\
      \                  (let ( (y (add1 z) ) ) x ))) "
      "1"
  ; t "let_10"
      "(let ( (x 1) (y 2) )\n\
      \                (let ( (z (add1 y)) )\n\
      \                  (let ( (x (add1 z) ) ) x ))) "
      "4"
  ; t "let_11"
      "(let ( (x 1) (y 2) )\n\
      \                (let ( (z (add1 y)) )\n\
      \                  (let ( (y (add1 z) ) ) z ))) "
      "3"
  ; t "let_12" "(let ((x (let ((y (add1 2))) (sub1 y)))) x)" "2"
  ; t "let_13" "(let ((x 5) (y x)) y)" "5"
  ; t "let_14" "(let ((x 5) (y (sub1 x))) (sub1 y))" "3"
  ; t "let_15" "(let ((x 5) (y (sub1 x)) (z (sub1 y))) (sub1 z))" "2"
  ; t "let_16" "(let ((x 5) (y 6) (z y)) z)" "6"
  ; t "let_17" "(let ((x (let ((x 5)) (sub1 x)))) x)" "4"
  ; (* scary nested let named expressions *)
    t "let_18"
      "(let ((z (let ( (x 1) (y 2) )\n\
      \                           (let ( (z (add1 y)) )\n\
      \                              (let ( (z (add1 z) ) ) z ))))) z) "
      "4"
  ; t "let_19"
      "(let ((z 1))\n\
      \        (let ( (z (let ((y 2)) \n\
      \                     (let ((z (add1 y)))\n\
      \                        (let ((z (add1 z))) z ))))) z))"
      "4"
  ; t "let_20"
      "(let ((x 1) (y (add1 x)))\n\
      \ \t     (let ((z (sub1 y)))\n\
      \              (add1 (sub1 y))))"
      "2"
    (* errors *)
  ; te "x_err" "x" "Unbound identifier"
  ; te "let_err_1" "(let ((x (add1 x))) (add1 x))" "Unbound identifier"
  ; te "let_err_2" "(let ((x (add1 y))) (add1 x))" "Unbound identifier"
  ; te "let_err_3" "(let ((x 2) (y 3) (x 5)) x)" "Variable x is redefined" ]

let scope_errors =
  [ te "scope_1" "(let ((x 10)) foo)" "Unbound identifier foo"
  ; te "scope_2" "(let ((x 10) (y z)) foo)" "Unbound identifier z"
  ; te "scope_3" "(let ((x 5) (x 6)) x)" "Variable x is redefined"
  ; te "scope_4" "(let ((x 1) (y 2) (x 0)) y)" "Variable x is redefined"
  ; te "scope_5" "(let ((let 10)) let)" "Reserved keyword let is redefined"
  ; te "scope_6" "(let ((sub1 10)) (sub1 sub1))"
      "Reserved keyword sub1 is redefined" ]

let syntax_errors =
  [ te "fail_0" "(1)" "Expecting let/add1/sub1 but received (1)"
  ; te "fail_1" "(x 1)" "Expecting let/add1/sub1 but received (x 1)"
  ; te "fail_2" "(1 1)" "Expecting let/add1/sub1 but received (1 1)"
  ; te "fail_3" "(let () 10)" "Expecting <bindings> but received ()"
  ; te "fail_4" "(let (x 10) 10)"
      "Expecting (IDENTIFIER <expr>) but received x"
  ; te "fail_5" "(let ((7 10)) x)"
      "Expecting (IDENTIFIER <expr>) but received (7 10)"
  ; te "fail_6" "(let ((x 1 2)) x)"
      "Expecting (IDENTIFIER <expr>) but received (x 1 2)"
  ; te "fail_7" "(let ((x 1)) x y)"
      "Expecting (let (<bindings>) <expr>) but received (let ((x 1)) x y)"
  ; te "fail_8" "(let ((x 1)) )"
      "Expecting (let (<bindings>) <expr>) but received (let ((x 1)))"
  ; te "fail_9" "(let x)"
      "Expecting (let (<bindings>) <expr>) but received (let x)"
  ; te "fail_10" "(let 1)"
      "Expecting (let (<bindings>) <expr>) but received (let 1)" ]

let all_tests = int_tests @ more_tests @ let_tests @ syntax_errors

let suite : OUnit2.test = "suite" >::: all_tests

let () = run_test_tt_main suite
