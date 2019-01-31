open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx";;

let t name program expected = name>::test_run program name expected;;

let ta name program expected = name>::test_run_anf program name expected;;

let te name program expected_err = name>::test_err program name expected_err;;

let tanf name program expected = name>::fun _ ->
  assert_equal expected (anf (tag program)) ~printer:string_of_expr;;

let teq name actual expected = name>::fun _ ->
  assert_equal expected actual ~printer:(fun s -> s);;

let tprog filename expected = filename>::test_run_input filename expected;;

let teprog filename expected = filename>::test_err_input filename expected;;

let forty_one = "41";;

(* simple boa programs for testing tag function *)

let n_1 = (ENumber(41, ()));;
let n_2 = (ENumber(5, ()));;
let id_1 = (EId("x", ()));;
let id_2 = (EId("y", ()));;

let prim1_1 = (EPrim1(Add1, n_1, ()));;
let prim1_2 = (EPrim1(Add1, id_1, ()));;
let prim1_3 = (EPrim1(Sub1, prim1_1, ()));;
let prim1_4 = (EPrim1 (Sub1, prim1_3, ()));;

let prim2_1 = (EPrim2(Plus, n_1, n_2, ()));;
let prim2_2 = (EPrim2(Plus, id_1, n_2, ()));;
let prim2_3 = (EPrim2(Times, prim2_1, prim2_2, ()));;
let prim2_4 = (EPrim2(Minus, prim2_3, prim2_1, ()));;

(* Add samples to test tagging in case of lets and ifs and some complex scenarios *)

(printf "Tagged1:\n%s\n" (format_expr (tag prim2_1) string_of_int));;
(printf "Tagged2:\n%s\n" (format_expr (tag prim2_2) string_of_int));;
(printf "Tagged3:\n%s\n" (format_expr (tag prim2_3) string_of_int));;
(printf "Tagged4:\n%s\n" (format_expr (tag prim2_4) string_of_int));;

let tag_tests = [
  teq "test_tag_n_1"  (format_expr (tag n_1) string_of_int) "ENumber<1>(41)" ;
  teq "test_tag_id_1"  (format_expr (tag id_1) string_of_int) "EId<1>(\"x\")" ;
  teq "test_tag_prim1_1"  (format_expr (tag prim1_1) string_of_int) "EPrim1<1>(Add1, ENumber<2>(41))" ;
  teq "test_tag_prim1_2"  (format_expr (tag prim1_2) string_of_int) "EPrim1<1>(Add1, EId<2>(\"x\"))" ;
  teq "test_tag_prim1_3"  (format_expr (tag prim1_3) string_of_int) "EPrim1<1>(Sub1, EPrim1<2>(Add1, ENumber<3>(41)))" ;
  teq "test_tag_prim1_4" (format_expr (tag prim1_4) string_of_int) "EPrim1<1>(Sub1, EPrim1<2>(Sub1, EPrim1<3>(Add1, ENumber<4>(41))))" ;
  teq "test_tag_prim2_1" (format_expr (tag prim2_1) string_of_int) "EPrim2<1>(Plus, ENumber<2>(41), ENumber<3>(5))";
  teq "test_tag_prim2_2" (format_expr (tag prim2_2) string_of_int) "EPrim2<1>(Plus, EId<2>(\"x\"), ENumber<3>(5))";

  teq "test_tag_prim2_3"
  (format_expr (tag prim2_3) string_of_int)
"EPrim2<1>(
  Times,
  EPrim2<2>(Plus, ENumber<3>(41), ENumber<4>(5)),
  EPrim2<5>(Plus, EId<6>(\"x\"), ENumber<7>(5))
)";
teq "test_tag_prim2_4"
(format_expr (tag prim2_4) string_of_int)
"EPrim2<1>(
  Minus,
  EPrim2<2>(
    Times,
    EPrim2<3>(Plus, ENumber<4>(41), ENumber<5>(5)),
    EPrim2<6>(Plus, EId<7>(\"x\"), ENumber<8>(5))
  ),
  EPrim2<9>(Plus, ENumber<10>(41), ENumber<11>(5))
)"
]

let suite =
"suite">:::
 [

  (*tanf "forty_one_anf"
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


  tprog "test1.boa" "3";

    (* Some useful if tests to start you off *)

  t "if1" "if 5: 4 else: 2" "4";
  t "if2" "if 0: 4 else: 2" "2";
*)
  ]
  @ tag_tests
;;


let () =
  run_test_tt_main suite
;;
