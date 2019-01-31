open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Types

let is_osx = Conf.make_bool "osx" false "Set this flag to run on osx"

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

let let_1 = ELet ([("x", n_1, ()); ("y", id_3, ())], prim2_5, ())

(* two simple bindings *)

let let_2 = ELet ([("x", prim1_3, ())], id_1, ())

(* binding with complex expression *)

let let_3 = ELet ([("z", let_2, ())], id_3, ())

(* nested let in the bind *)

let let_4 = ELet ([("y", let_2, ())], let_3, ())

(* nested in the binding and in the body  *)

(* Add samples to test tagging in case of lets and ifs and some complex scenarios *)

;;
(*
printf "Tagged1:\n%s\n\n" (format_expr (tag let_1) string_of_int)

;;
printf "Tagged2:\n%s\n\n" (format_expr (tag let_2) string_of_int)

;;
printf "Tagged3:\n%s\n\n" (format_expr (tag let_3) string_of_int)

;;
printf "Tagged4:\n%s\n\n" (format_expr (tag let_4) string_of_int) *)

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
       )" ]

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


  tprog "test1.boa" "3";

    (* Some useful if tests to start you off *)

  t "if1" "if 5: 4 else: 2" "4";
  t "if2" "if 0: 4 else: 2" "2";
*) ]
       @ tag_tests

let () = run_test_tt_main suite
