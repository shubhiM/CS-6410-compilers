open List
   
(*

  This is a datatype that describes simple algebra expressions.

  For example, the expression

    5 * 3 + y
  
  would be represented as

    Plus(Times(Num(5), Num(3)), Variable("y"))

  Note that the multiplication is inside the addition because the order of
  operations demands that we do it first.


  Here are some other examples:

    1 + x               Plus(Num(1), Variable("x"))
    x + x               Plus(Variable("x"), Variable("x"))
    5(y + 10)           Times(Num(5), Plus(Variable("y"), Num(10)))

*)

type arith =
  | Plus of arith * arith
  | Times of arith * arith
  | Variable of string
  | Num of int


(**
  To represent the set of known variables, we're going to use a 
  simple list representation: (string * int) list
 *)
type env = (string * int) list

(**
  To use such an environment, we need to be able to `add` new variables
  to an environment (which produces a *new* environment), and we need
  to `get` the values of variables in an environment. We may also want
  to check whether an environment `contains` a particular variable.

  Notice that I described this as a *set*, which implies no duplicate
  variable names, but the list type above can certainly have such
  duplicates.  You need to decide how to handle such duplication.
  One solution will be markedly easier than the alternatives!

  Additionally, for `get`, we need to handle the case where the variable 
  is not found.
 *)

(* Implement the following three functions.  If needed, you may define them
   recursively, so uncomment out the `rec` keyword. *)
let get (env : env) (x : string) : int option =
  assoc_opt x env
;;
let contains (env : env) (x : string) : bool =
  mem_assoc x env
;;
let add (env : env) (x : string) (xVal : int) : env =
  (* How do we handle duplicates here
      we can check if we already have the key and then ignore the addition
      we can update the kvpair
      we can report an error
   this depends on how we visualize our environment to be *)
   env @ [(x, xVal)]
 ;;
             
(*
  Next, write evaluate, which takes an arithmetic expression and 
  an environment mapping from strings to integers, and evaluate the expression,
  using the given integer value for the variable.
  
  For example
  
     evaluate
       (Times(Plus(Variable("x"), Variable("y")), Num(5)))
       [("x", 5); ("y", 7)]

  should produce 60, and

     evaluate (Plus(Num(4), Num(5))) []

  should produce 9.
  
  If there is a variable not contained in vars in the expression, throw an
  exception with failwith.

*)

let rec evaluate (a : arith) (vars : env) : int =
  match a with
  | Plus (left, right) ->
     (evaluate left vars) + (evaluate right vars)
  | Times (left, right) ->
     (evaluate left vars) * (evaluate right vars)
  | Num n -> n
  | Variable y ->
        let pair_option = (get vars y) in
        match pair_option with
          | None -> failwith "variable not found"
          | Some c -> c;;
            
(*
  Next, write pretty, which takes an arithmetic expression and renders it in
  mathematical notation.

  It should print with minimal parentheses, assuming standard order of
  operations where multiplication binds more tightly than addition.

  Further, if there is a multiplication of a variable, it should be
  pretty-printed by putting the coefficient adjacent, for example:

    pretty (Plus(Plus(Times(Plus(Num(5), Variable("y")), Variable("x")), Num(2)), Num(1)))
  
  should pretty-print as

    (5 + y)x + 2 + 1

  HINT: it may be helpful to write a helper that keeps track of whether the
  current expression is part of of plus or times expression as an additional
  argument.

  NOTE: I expect lots of questions about "how pretty" your solution "has" to
  be.  See how well you can do – I'm not giving a formal specification of
  exactly what form you need to produce, though examples like the one above
  should work nicely.  There are several reasonable answers here.
*)

let rec pretty (a : arith) : string =
  ""



