open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types

let is_osx = Conf.make_bool "osx" true "Set this flag to run on osx"

let t name program expected = name >:: test_run program name expected

let ta name program expected = name >:: test_run_anf program name expected

let te name program expected_err = name >:: test_err program name expected_err

let tanf name program expected =
  name
  >:: fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_expr

let teq name actual expected =
  name >:: fun _ -> assert_equal expected actual ~printer:(fun s -> s)

let tprog filename expected = filename >:: test_run_input filename expected

let teprog filename expected = filename >:: test_err_input filename expected

let forty_one = "41"

(* Examples to test tagging of Boa's AST *)

let n_1 = ENumber (41, ())

let n_2 = ENumber (5, ())

let id_1 = EId ("x", ())

let id_2 = EId ("y", ())

let id_3 = EId ("z", ())

let prim1_1 = EPrim1 (Add1, n_1, ())

let prim1_2 = EPrim1 (Add1, id_1, ())

let prim1_3 = EPrim1 (Sub1, prim1_1, ())

let prim1_4 = EPrim1 (Sub1, prim1_3, ())

let prim2_1 = EPrim2 (Plus, n_1, n_2, ())

let prim2_2 = EPrim2 (Plus, id_1, n_2, ())

let prim2_3 = EPrim2 (Times, prim2_1, prim2_2, ())

let prim2_4 = EPrim2 (Minus, prim2_3, prim2_1, ())

let prim2_5 = EPrim2 (Plus, EId ("x", ()), EId ("y", ()), ())

(* two simple bindings *)
let let_1 = ELet ([("x", n_1, ()); ("y", id_3, ())], prim2_5, ())

(* binding with complex expression *)
let let_2 = ELet ([("x", prim1_3, ())], id_1, ())

(* nested let in the bind *)
let let_3 = ELet ([("z", let_2, ())], id_3, ())

(* nested in the binding and in the body  *)
let let_4 = ELet ([("y", let_2, ())], let_3, ())

let if_1 = EIf (prim1_4, prim2_3, let_1, ())

let int_tests =
  [ t "int_1" "41" "41"
  ; t "int_2" "2147483647" "2147483647"
  ; t "int_3" "add1(2147483647)" "-2147483648"
  ; t "add1_5" "add1(5)" "6"
  ; t "sub1_5" "sub1(5)" "4"
  ; t "add1_neg5" "add1(-5)" "-4"
  ; t "sub1_neg5" "sub1(-5)" "-6"
  (*; t "sub1_sub1" "(sub1 (sub1 5))" "3"
  ; t "add1_sub1" "(add1 (sub1 5))" "5"
  ; t "sub1_add1_sub1" "(sub1 (add1 (sub1 5)))" "4" *) ]


let let_tests =
    [ t "let_0" "let x = 10 in x" "10"
    ; t "let_1" "let x = 2 in add1(x)" "3"
    ; t "let_2" "let x = add1(2) in add1(x)" "4"
    ; t "let_3" "let x = 3 in let y = 4 in add1(x)" "4"
    ; t "let_4" "let x = 3 in let y = 4 in add1(y)" "5"
    ; t "let_5" "let x = 3 in let y = 4 in let z = 10 in let w = 8 in w" "8"
    ; t "let_6" "let x = 2 in let y = add1(x) in add1(y)" "4"

   (* TODO: uncomment once anf function is completely done *)
   (*; t "let_3" "(let ((x 3) (y 4)) (add1 x))" "4"
   ; t "let_4" "(let ((x 3) (y 4)) (add1 y))" "5"
   ; t "let_5" "(let ((x 3) (y 4) (z 10) (w 8)) w)" "8" *)

    (* TODO: check if the shadowing of variables with same name is permitted in the boa language *)
    ; t "let_7" "let x = 2 in let x = add1(x) in x" "3"
    (*; t "let_8" "let x = add1(2) in add1(sub1(x))" "3" *)
]

(*  let more_tests =
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
*)


let tag_tests =
  [ teq "test_tag_n_1" (format_expr (tag n_1) string_of_int) "ENumber<1>(41)"
  ; teq "test_tag_id_1" (format_expr (tag id_1) string_of_int) "EId<1>(\"x\")"
  ; teq "test_tag_prim1_1"
      (format_expr (tag prim1_1) string_of_int)
      "EPrim1<1>(Add1, ENumber<2>(41))"
  ; teq "test_tag_prim1_2"
      (format_expr (tag prim1_2) string_of_int)
      "EPrim1<1>(Add1, EId<2>(\"x\"))"
  ; teq "test_tag_prim1_3"
      (format_expr (tag prim1_3) string_of_int)
      "EPrim1<1>(Sub1, EPrim1<2>(Add1, ENumber<3>(41)))"
  ; teq "test_tag_prim1_4"
      (format_expr (tag prim1_4) string_of_int)
      "EPrim1<1>(Sub1, EPrim1<2>(Sub1, EPrim1<3>(Add1, ENumber<4>(41))))"
  ; teq "test_tag_prim2_1"
      (format_expr (tag prim2_1) string_of_int)
      "EPrim2<1>(Plus, ENumber<2>(41), ENumber<3>(5))"
  ; teq "test_tag_prim2_2"
      (format_expr (tag prim2_2) string_of_int)
      "EPrim2<1>(Plus, EId<2>(\"x\"), ENumber<3>(5))"
  ; teq "test_tag_prim2_3"
      (format_expr (tag prim2_3) string_of_int)
      "EPrim2<1>(\n\
      \  Times,\n\
      \  EPrim2<2>(Plus, ENumber<3>(41), ENumber<4>(5)),\n\
      \  EPrim2<5>(Plus, EId<6>(\"x\"), ENumber<7>(5))\n\
       )"
  ; teq "test_tag_prim2_4"
      (format_expr (tag prim2_4) string_of_int)
      "EPrim2<1>(\n\
      \  Minus,\n\
      \  EPrim2<2>(\n\
      \    Times,\n\
      \    EPrim2<3>(Plus, ENumber<4>(41), ENumber<5>(5)),\n\
      \    EPrim2<6>(Plus, EId<7>(\"x\"), ENumber<8>(5))\n\
      \  ),\n\
      \  EPrim2<9>(Plus, ENumber<10>(41), ENumber<11>(5))\n\
       )"
  ; teq "test_tag_let_1"
      (format_expr (tag let_1) string_of_int)
      "ELet<1>(\n\
      \  (( \"x\"<2>, ENumber<3>(41)), ( \"y\"<4>, EId<5>(\"z\"))),\n\
      \  EPrim2<6>(Plus, EId<7>(\"x\"), EId<8>(\"y\"))\n\
       )"
  ; teq "test_tag_let_2"
      (format_expr (tag let_2) string_of_int)
      "ELet<1>(\n\
      \  (( \"x\"<2>, EPrim1<3>(Sub1, EPrim1<4>(Add1, ENumber<5>(41))))),\n\
      \  EId<6>(\"x\")\n\
       )"
  ; teq "test_tag_let_3"
      (format_expr (tag let_3) string_of_int)
      "ELet<1>(\n\
      \  (\n\
      \    ( \"z\"<2>,\n\
      \      ELet<3>(\n\
      \        (( \"x\"<4>, EPrim1<5>(Sub1, EPrim1<6>(Add1, ENumber<7>(41))))),\n\
      \        EId<8>(\"x\")\n\
      \      )\n\
      \    )\n\
      \  ),\n\
      \  EId<9>(\"z\")\n\
       )"
  ; teq "test_tag_let_4"
      (format_expr (tag let_4) string_of_int)
      "ELet<1>(\n\
      \  (\n\
      \    ( \"y\"<2>,\n\
      \      ELet<3>(\n\
      \        (( \"x\"<4>, EPrim1<5>(Sub1, EPrim1<6>(Add1, ENumber<7>(41))))),\n\
      \        EId<8>(\"x\")\n\
      \      )\n\
      \    )\n\
      \  ),\n\
      \  ELet<9>(\n\
      \    (\n\
      \      ( \"z\"<10>,\n\
      \        ELet<11>(\n\
      \          (( \"x\"<12>, EPrim1<13>(Sub1, EPrim1<14>(Add1, \
       ENumber<15>(41))))),\n\
      \          EId<16>(\"x\")\n\
      \        )\n\
      \      )\n\
      \    ),\n\
      \    EId<17>(\"z\")\n\
      \  )\n\
       )"
  ; teq "test_tag_if_1"
      (format_expr (tag if_1) string_of_int)
      "EIf<1>(\n\
      \  EPrim1<2>(Sub1, EPrim1<3>(Sub1, EPrim1<4>(Add1, ENumber<5>(41)))),\n\
      \  EPrim2<6>(\n\
      \    Times,\n\
      \    EPrim2<7>(Plus, ENumber<8>(41), ENumber<9>(5)),\n\
      \    EPrim2<10>(Plus, EId<11>(\"x\"), ENumber<12>(5))\n\
      \  ),\n\
      \  ELet<13>(\n\
      \    (( \"x\"<14>, ENumber<15>(41)), ( \"y\"<16>, EId<17>(\"z\"))),\n\
      \    EPrim2<18>(Plus, EId<19>(\"x\"), EId<20>(\"y\"))\n\
      \  )\n\
       )" ]

let already_anfed_tests = [
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

    (* Multiplication is not working as expected *)

  (*  t "t_12" "0 * 5 " "0";
    t "t_13" "1 * 5"  "5"; *)

    te "t_14" "x * y" "Name x not found";
    te "t_15" "1 * y" "Name y not found";
    t "t_16" "1 - 1" "0";
    t "t_17" "1 - -10" "11";

    t "t_18" "if 5: add1(4) else: 2" "5";
    t "t_19" "if 0: 4 else: sub1(2)" "1";

    tprog "test_2.boa" "3";
    tprog "test_3.boa" "2";
    t "t_21" "let x=2 in add1(x)" "3";
    t "t_20" "let x=2 in let y = add1(x) in sub1(y)" "2";
]
let suite =
  "suite"
  >::: [ (*tanf "forty_one_anf"
       (ENumber(41, ()))
       forty_one_a;

  tanf "prim1_anf"
       (EPrim1(Sub1, ENumber(55, ()), ()))
       (ELet(["unary_1", EPrim1(Sub1, ENumber(55, ()), ()), ()],
             EId("unary_1", ()),
             ()));
 *)
         (*ta "forty_one_run_anf" (tag forty_one_a) "41";*)

         (*t "forty_one" forty_one "41";

*)


    (* Some useful if tests to start you off *)


 ]
(*@ tag_tests
@ int_tests
@ let_tests *)
@ already_anfed_tests
let () = run_test_tt_main suite
