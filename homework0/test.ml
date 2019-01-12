open OUnit2
open Functions

(* a helper for testing integers *)
let t_int name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int);;

(* a helper for testing strings *)
let t_string name value expected =name>::
  (fun _ -> assert_equal expected value);; 

let fib_0 = t_int "fib_0" (fibonnaci 0) 0;;
let fib_1 = t_int "fib_1" (fibonnaci 1) 1;;
let fib_2 = t_int "fib_2" (fibonnaci 2) 1;;
let fib_10 = t_int "fib_10" (fibonnaci 10) 55;;
let max_first = t_int "max_first" (max 3 2) 3;;
let max_second = t_int "max_second" (max 2 3) 3;;
let max_equal = t_int "max_equal" (max 0 0) 0;;

(* binary tree definitions for tests *)
let single_node_bt1 = Node("a", Leaf, Leaf);;
let single_node_bt2 = Node("b", Leaf, Leaf);;
let single_node_bt3 = Node("c", Leaf, Leaf);;

let balanced_bt = Node("a", single_node_bt2, single_node_bt3);;
let left_skewed_bt = Node("a", single_node_bt2, Leaf);;
let right_skewed_bt = Node("a", Leaf, single_node_bt3);;
let complex_bt = Node("a",  left_skewed_bt, right_skewed_bt);;

(* testing inorder_str *)
let inorder_empty_bt =
  t_string "inorder_empty_bt" (inorder_str Leaf) "";;
let inorder_single_node_bt =
  t_string "inorder_single_node_bt" (inorder_str single_node_bt1) "a";;
let inorder_balanced_bt =
  t_string "inorder_balanced_bt" (inorder_str balanced_bt) "bac";;
let inorder_left_skewed_bt =
  t_string "inorder_left_skewed_bt" (inorder_str left_skewed_bt) "ba";;
let inorder_right_skewed_bt =
  t_string "inorder_right_skewed_bt" (inorder_str right_skewed_bt) "ac";;
let inorder_complex_bt =
  t_string "inorder_complex_bt" (inorder_str complex_bt) "baaac";;

(* tests for size *)
let size_empty_bt =
  t_int "size_empty_bt" (size Leaf) 0;;
let size_single_node_bt =
  t_int "size_single_node_bt" (size single_node_bt1) 1;;
let size_balanced_bt =
  t_int "size_balanced_bt" (size balanced_bt) 3;;
let size_left_skewed_bt =
  t_int "size_left_skewed_bt" (size left_skewed_bt) 2;;
let size_right_skewed_bt =
  t_int "size_right_skewed_bt" (size right_skewed_bt) 2;;
let size_complex_bt =
  t_int "size_complex_bt" (size complex_bt) 5;;

(* tests for height *)
let height_empty_bt =
  t_int "height_empty_bt" (height Leaf) 0;;
let height_single_node_bt =
  t_int "height_single_node_bt" (height single_node_bt1) 1;;
let height_balanced_bt =
  t_int "height_balanced_bt" (height balanced_bt) 2;;
let height_left_skewed_bt =
  t_int "height_left_skewed_bt" (height left_skewed_bt) 2;;
let height_right_skewed_bt =
  t_int "height_right_skewed_bt" (height right_skewed_bt) 2;;
let height_complex_bt =
  t_int "height_complex_bt" (height complex_bt) 3;;

(* tests for increment_all *)
let empty_list = [];;
let single_element_list = [1];;
let long_list = [1; 3; 5; 7; 9; 11; 13; 15];;

let incr_all_empty_list _  =
  assert_equal (increment_all empty_list) [];;
let incr_all_single_ele_list _ =
  assert_equal (increment_all single_element_list) [2];;
let incr_all_long_list _ =
  assert_equal (increment_all long_list) [2; 4; 6; 8; 10; 12; 14; 16];;

let increment_all_empty_list =
  "increment_all_empty_list">::incr_all_empty_list;;
let increment_all_single_element_list =
  "increment_all_single_element_list">::incr_all_single_ele_list;;
let increment_all_long_list =
  "increment_all_long_list">::incr_all_long_list;;

(* tests for long strings *)
let str_empty_list = [];;
let str_long_list = ["one"; "two"; "three"; "four"; "five"; "six"];;

let long_strs_empty_list _  =
  assert_equal (long_strings str_empty_list 0) [];;
let long_strs_long_list _ =
  assert_equal (long_strings str_long_list 3)  ["three"; "four"; "five"];;
let long_strs_no_solution _ =
  assert_equal (long_strings str_long_list 5)  [];;
let long_strs_long_list _ =
  assert_equal (long_strings str_long_list 3)  ["three"; "four"; "five"];;

let long_strings_empty_list =
  "long_strings_empty_list">::long_strs_empty_list;;
let long_strings_no_solution =
  "long_strings_no_solution">::long_strs_no_solution;;
let long_strings_long_list =
  "long_strings_long_list">::long_strs_long_list;;

(* tests for  every other *)

let every_othr_empty_list _  =
  assert_equal (every_other str_empty_list) [];;
let every_othr_long_str_list _  =
  assert_equal (every_other str_long_list) ["one"; "three"; "five"];;
let every_othr_long_list _  =
  assert_equal (every_other long_list) [1; 5; 9; 13];;
let every_othr_single_ele_list _  =
  assert_equal (every_other single_element_list) [1];;

let every_other_empty_list =
  "every_other_empty_list">::every_othr_empty_list;;
let every_other_long_str_list =
  "every_other_long_str_list">::every_othr_long_str_list;;
let every_other_long_list =
  "every_other_long_list">::every_othr_long_list;;
let every_other_single_ele_list =
  "every_other_single_ele_list">::every_othr_single_ele_list;;

(* tests for sum_all *)
let list1 = [[1]];;
let list2 = [];;
let list3 = [[];[];[]];;
let list4 = [[1; 1; 1]; [2; 2; 2]; [3; 3; 3]];;

let sum_all_l_1 _ = 
  assert_equal (sum_all list1) [1];;
let sum_all_l_2 _ = 
  assert_equal (sum_all list2) [];;
let sum_all_l_3 _ = 
  assert_equal (sum_all list3) [0; 0; 0];;
let sum_all_l_4 _ = 
  assert_equal (sum_all list4) [3; 6; 9];;

let sum_all_list1 = 
  "sum_all_list1">::sum_all_l_1;;
let sum_all_list2 = 
  "sum_all_list2">::sum_all_l_2;;
let sum_all_list3 = 
  "sum_all_list3">::sum_all_l_3;;
let sum_all_list4 = 
  "sum_all_list4">::sum_all_l_4;;

let suite = "suite">:::[
  (* testing fibonnaci *)
  fib_0;
  fib_1;
  fib_2;
  fib_10;
  (* testing max *)
  max_first;
  max_second;
  max_equal;
  (* testing inorder_str *)
  inorder_empty_bt;
  inorder_single_node_bt;
  inorder_balanced_bt;
  inorder_left_skewed_bt;
  inorder_right_skewed_bt;
  inorder_complex_bt;
  (* testing size *)
  size_empty_bt;
  size_single_node_bt;
  size_balanced_bt;
  size_left_skewed_bt;
  size_right_skewed_bt;
  size_complex_bt;
  (* testing height *)
  height_empty_bt;
  height_single_node_bt;
  height_balanced_bt;
  height_left_skewed_bt;
  height_right_skewed_bt;
  height_complex_bt;
  (* testing increment_all *)
  increment_all_empty_list;
  increment_all_single_element_list;
  increment_all_long_list;
  (* testing long_strings *)
  long_strings_empty_list;
  long_strings_long_list;
  long_strings_no_solution;
  (* testing every_other *)
  every_other_empty_list;
  every_other_long_str_list;
  every_other_long_list;
  every_other_single_ele_list;
  (* testing_sum_all *)
  sum_all_list1;
  sum_all_list2;
  sum_all_list3;
  sum_all_list4;     
  ];;

run_test_tt_main suite
