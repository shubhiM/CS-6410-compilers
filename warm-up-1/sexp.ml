open Unix
open Str
open Printf
open List

type 'a tok =
  | LPAREN of 'a
  | RPAREN of 'a
  | TSym of string * 'a
  | TInt of int * 'a
  | TBool of bool * 'a

let tok_info t =
  match t with
  | LPAREN x -> x
  | RPAREN x -> x
  | TSym (_, x) -> x
  | TInt (_, x) -> x
  | TBool (_, x) -> x

(* startline, startcol, endline, endcol *)
type pos = int * int * int * int

let pos_to_string (startline, startcol, endline, endcol) range =
  if range then
    Printf.sprintf "line %d, col %d--line %d, col %d" startline startcol
      endline endcol
  else Printf.sprintf "line %d, col %d" startline startcol

(* tokenize parses the program string and converts it into a list of 
   (position , token) tuples where a token is of type tok and position is of 
   type pos. Following is the order of work performed by the function.

  1. converts the given input string to a split_result list by using full_split
     library function. (full_split (regexp "[()\n\t ]") str) call matches the
     regexp in str and splits str including the matched regular expression. The
     final list thus retains both the delimiters and the text. the resulting
     list is composed of the elements of type 'split_result'.
     type split_result = 
      |	Text of string
      |	Delim of string
  
   2. uses List.fold_left library function to fold over the list from step 1 and
      accumulate the final list of (position, token) tuples.
      
      Signature of List.fold_left function
      
      List.fold_left (f : 'a -> 'b ->'a) (acc : 'a) (l : 'b list): 'a 
      
      where f is the fold_function that gets applied from left to right on 
      the input list and result gets accumulated for each iteration in an 
      accumulator consumed by the fold function. A seed value is used 
      as the initial value for the accumulator. When all of the list is 
      consumed, the value in accumulator is returned as final result.

      fold_function in tokenize has the following signature.
      (fun ((toks : pos tok list), (line : int), (col : int))
            (tok : Str.split_result) 
      
      It takes a two arguments
      The accumulator of type ((toks : pos tok list), (line : int), (col : int))
      and the element from the list of type Str.split_result as explained above.

      The seed value for the accumulator consists of empty list and a line and
      col int field both initialized to 0 to correspond to the starting position
      in the original string. (The original string can be thought of as organized
      as a 2D array with each character occupying a cell in the array and starting
      index set to line - 0 and col - 0)
      
      The function then checks if the input tok is a delimiter or actual text and
      according adds or drops the input tok in the final list.
      
      Incase of delimiters , it ignores the " ", "\n", "\t" tok values and 
      adds "(" ")" along with their position in the input string.

      Incase of text, it adds the booleans along with their position in the 
      accumulator. If the tok is not a boolean then the function explicitly 
      tries to convert the symbol into an integer and raises an exception
      if the tok cannot be converted to an integer value, it then uses the tok
      as a Symbol in our language.

      The following code accomplishes this conversion
      try ((TInt (int_of_string t, (line, col, line, col + tLen))) :: toks, line, col + tLen) with
        | Failure _ -> (TSym (t, (line, col, line, col + tLen)) :: toks, line, col + tLen)
      
      At the end we ignore the line and col value as it is no longer important
      and collect all the tokens in toks
       
      Since fold_left applies the function from left to right, we reverse the
      the output toks from fold_left to preserve the order of tokens in the
      original string. 
 *)

let tokenize (str : string) : pos tok list =
  let toks, _, _ =
    List.fold_left
      (fun ((toks : pos tok list), (line : int), (col : int))
           (tok : Str.split_result) ->
        match tok with
        | Delim t ->
            if t = " " then (toks, line, col + 1)
            else if t = "\t" then (toks, line, col + 1)
            else if t = "\n" then (toks, line + 1, 0)
            else if t = "(" then
              (LPAREN (line, col, line, col + 1) :: toks, line, col + 1)
            else if t = ")" then
              (RPAREN (line, col, line, col + 1) :: toks, line, col + 1)
            else
              let tLen = String.length t in
              ( TSym (t, (line, col, line, col + tLen)) :: toks
              , line
              , col + tLen )
        | Text t -> (
            if t = "true" then
              (TBool (true, (line, col, line, col + 4)) :: toks, line, col + 4)
            else if t = "false" then
              (TBool (false, (line, col, line, col + 5)) :: toks, line, col + 5)
            else
              let tLen = String.length t in
              try
                ( TInt (int_of_string t, (line, col, line, col + tLen)) :: toks
                , line
                , col + tLen )
              with Failure _ ->
                ( TSym (t, (line, col, line, col + tLen)) :: toks
                , line
                , col + tLen ) ) )
      ([], 0, 0)
      (full_split (regexp "[()\n\t ]") str)
  in
  List.rev toks

type 'a sexp =
  | Sym of string * 'a
  | Int of int * 'a
  | Bool of bool * 'a
  | Nest of 'a sexp list * 'a

let sexp_info s =
  match s with
  | Sym (_, x) -> x
  | Int (_, x) -> x
  | Bool (_, x) -> x
  | Nest (_, x) -> x

type ('a, 'b) result = Ok of 'a | Error of 'b

let token_to_sexpr (t : pos tok) : pos sexp =
  match t with
  | TSym (sym, sp) -> Sym (sym, sp)
  | TInt (i, ip) -> Int (i, ip)
  | TBool (b, bp) -> Bool (b, bp)
  | _ -> failwith "Expecting only bool, symbol or integer token"

let update_nest_pos (nest : pos sexp) (np : pos) : pos sexp =
  match nest with
  | Nest (sexprs, p) -> Nest (sexprs, np)
  | _ -> failwith "Expecting nest sexpression in input"

let update_nest_sexpr (nest : pos sexp) (exp : pos sexp) : pos sexp =
  match nest with
  | Nest (sexprs, p) -> Nest (sexprs @ [exp], p)
  | _ -> failwith "Expecting nest sexpression in input"

let get_nest_pos (nest : pos sexp) : pos =
  match nest with
  | Nest (sexprs, p) -> p
  | _ -> failwith "Expecting nest sexpression in input"

let rec parse_helper (toks : pos tok list) (sexprs : pos sexp list)
    (nests : pos sexp list) : (pos sexp list, string) result =
  match toks with
  | [] ->
      if length nests == 0 then Ok sexprs
      else
        let n = hd nests in
        let msg1 =
          "Mismatching left paranthesis found at "
          ^ pos_to_string (get_nest_pos n) false
        in
        Error msg1
  | t :: ts -> (
    match t with
    | LPAREN p ->
        let new_nest = Nest ([], p) in
        parse_helper ts sexprs (cons new_nest nests)
    | RPAREN p ->
        let num_of_nests = length nests in
        if num_of_nests == 0 then
          let msg =
            "Mismatching right paranthesis found at " ^ pos_to_string p false
          in
          Error msg
        else
          let new_nest = update_nest_pos (hd nests) p in
          if num_of_nests == 1 then
            parse_helper ts (sexprs @ [new_nest]) (tl nests)
          else
            let rest_of_nests = tl nests in
            let parent_nest = update_nest_sexpr (hd rest_of_nests) new_nest in
            let new_nests = cons parent_nest (tl rest_of_nests) in
            parse_helper ts sexprs new_nests
    | _ ->
        let t_sexpr = token_to_sexpr t in
        if length nests == 0 then parse_helper ts (sexprs @ [t_sexpr]) nests
        else
          let curr_nest = hd nests in
          let new_nest = update_nest_sexpr curr_nest t_sexpr in
          parse_helper ts sexprs (cons new_nest (tl nests)) )

let parse_toks (toks : pos tok list) : (pos sexp list, string) result =
  parse_helper toks [] []

let parse (str : string) : (pos sexp list, string) result =
  let toks = tokenize str in
  parse_toks toks
