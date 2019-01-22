open Printf
open Sexp

(* Abstract syntax of (a small subset of) x86 assembly instructions *)

type reg = EAX | ESP

type arg = Const of int | Reg of reg | RegOffset of int * reg

(* int is # words of offset *)

type instruction = IMov of arg * arg | IAdd of arg * arg | IRet

(* Concrete syntax of the Adder language:

‹expr›:
  | NUMBER
  | IDENTIFIER
  | ( let ( ‹bindings› ) ‹expr› )
  | ( add1 ‹expr› )
  | ( sub1 ‹expr› )
‹bindings›:
  | ( IDENTIFIER ‹expr› )
  | ( IDENTIFIER ‹expr› ) ‹bindings›

 *)

(* Abstract syntax of the Adder language *)

type prim1 = Add1 | Sub1

type 'a expr =
  | Number of int * 'a
  | Id of string * 'a
  | Let of (string * 'a expr) list * 'a expr * 'a
  | Prim1 of prim1 * 'a expr * 'a

(* The exception to be thrown when some sort of problem is found with names *)
exception BindingError of string

(* The exception to be thrown when some sort of problem is found with syntax *)
exception SyntaxError of string

let rec string_of_sexp s =
  match s with
  | Sym (x, _) -> x
  | Int (n, _) -> string_of_int n
  | Bool (_, _) -> failwith "Not implemented"
  | Nest (sexps, _) ->
      List.fold_left
        (fun str sexp ->
          str
          ^ (if String.length str != 1 then " " else "")
          ^ string_of_sexp sexp )
        "(" sexps
      ^ ")"

let syntax_error (msg : string) info =
  raise (SyntaxError (msg ^ " at " ^ pos_to_string info true))

(* Function to convert from unknown s-expressions to Adder exprs
   Throws a SyntaxError message if there's a problem
 *)
let rec expr_of_sexp (s : pos sexp) : pos expr =
  let expr_of_bindings bindings =
    List.fold_left
      (fun exprs sexp ->
        match sexp with
        | Nest ([Sym (x, info'); expr], info) ->
            if x = "let" || x = "add1" || x = "sub1" then
              raise
                (BindingError
                   ( "Reserved keyword " ^ x ^ " is redefined at "
                   ^ pos_to_string info' true ))
            else if List.exists (fun (y, _) -> x = y) exprs then
              raise
                (BindingError
                   ( "Variable " ^ x ^ " is redefined at "
                   ^ pos_to_string info' true ))
            else
              let let_expr = (x, expr_of_sexp expr) in
              exprs @ [let_expr]
        | _ ->
            syntax_error
              ( "Expecting (IDENTIFIER <expr>) but received "
              ^ string_of_sexp sexp )
              (sexp_info sexp) )
      [] bindings
  in
  match s with
  | Sym (id, info) -> Id (id, info)
  | Int (n, info) -> Number (n, info)
  | Bool (value, info) -> failwith "Not implemented"
  | Nest (sexps, info) -> (
    match sexps with
    | [Sym ("let", _); (Nest (bindings, info') as bs); b] ->
        if List.length bindings != 0 then
          let exprs = expr_of_bindings bindings in
          Let (exprs, expr_of_sexp b, info)
        else
          syntax_error
            ("Expecting <bindings> but received " ^ string_of_sexp bs)
            info'
    | [Sym ("add1", _); b] -> Prim1 (Add1, expr_of_sexp b, info)
    | [Sym ("sub1", _); b] -> Prim1 (Sub1, expr_of_sexp b, info)
    | Sym ("let", _) :: rest ->
        syntax_error
          ( "Expecting (let (<bindings>) <expr>) but received "
          ^ string_of_sexp s )
          info
    | Sym ("add1", _) :: rest ->
        syntax_error
          ("Expecting (add1 <expr>) but received " ^ string_of_sexp s)
          info
    | Sym ("sub1", _) :: rest ->
        syntax_error
          ("Expecting (sub1 <expr>) but received " ^ string_of_sexp s)
          info
    | _ ->
        syntax_error
          ("Expecting let/add1/sub1 but received " ^ string_of_sexp s)
          info )

(* Functions that implement the compiler *)

(* The next four functions convert assembly instructions into strings,
   one datatype at a time.  Only one function has been fully implemented
   for you. *)
let reg_to_asm_string (r : reg) : string =
  (* COMPLETE THIS FUNCTION *)
  match r with EAX -> sprintf "eax" | ESP -> sprintf "esp"

let arg_to_asm_string (a : arg) : string =
  match a with
  | Const n -> sprintf "%d" n
  | Reg reg -> reg_to_asm_string reg
  | RegOffset (offset, reg) ->
      sprintf "[%s - %d]" (reg_to_asm_string reg) (offset * 4)

let instruction_to_asm_string (i : instruction) : string =
  match i with
  | IMov (dest, value) ->
      sprintf "\tmov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
  | IRet -> sprintf "\tret"
  | IAdd (dest, value) ->
      sprintf "\tadd %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)

let to_asm_string (is : instruction list) : string =
  List.fold_left
    (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i))
    "" is

(* A helper function for looking up a name in a "dictionary" and 
   returning the associated value if possible.  This is definitely
   not the most efficient data structure for this, but it is nice and simple... *)
let rec find (ls : (string * 'a) list) (x : string) : 'a option =
  match ls with
  | [] -> None
  | (y, v) :: rest -> if y = x then Some v else find rest x

(* The actual compilation process.  The `compile` function is the primary function,
   and uses `compile_env` as its helper.  In a more idiomatic OCaml program, this
   helper would likely be a local definition within `compile`, but separating it out
   makes it easier for you to test it. *)
let rec compile_env (p : pos expr)
    (* the program, currently annotated with source location info *)
    (stack_index : int)
    (* the next available stack index *)
    (env : (string * int) list) : instruction list =
  (* the current binding environment of names to stack slots *)
  (* the instructions that would execute this program *)
  match p with
  | Number (n, _) -> [IMov (Reg EAX, Const n)]
  | Id (v, _) -> (
      let stack_slot = find env v in
      match stack_slot with
      | None -> raise (BindingError (sprintf "Unbound identifier %s" v))
      | Some a -> [IMov (Reg EAX, RegOffset (a, ESP))] )
  | Prim1 (Add1, e, _) ->
      compile_env e stack_index env @ [IAdd (Reg EAX, Const 1)]
  | Prim1 (Sub1, e, _) ->
      compile_env e stack_index env @ [IAdd (Reg EAX, Const (-1))]
  | Let (bindings, body, _) ->
      (* compile_bindings returns a list of instructions for named-expressions and
       updates the environment with bindings *)
      let is, new_env, nxt_stack_index =
        compile_bindings bindings stack_index env
      in
      is @ compile_env body nxt_stack_index new_env

and compile_bindings (bs : (string * pos expr) list)
    (* list of named expressions in let expression *)
    (si : int)  (* current stack index *)
               (env : (string * int) list) :
    (* environment in which named-expressions are compiled *)
    instruction list * (string * int) list * int =
  match bs with
  | [] -> ([], env, si)
  | (v, exp) :: rs ->
      let iexp =
        compile_env exp si env @ [IMov (RegOffset (si, ESP), Reg EAX)]
      in
      let irest, env_rest, next_stack_index =
        compile_bindings rs (si + 1) ([(v, si)] @ env)
      in
      let iall = iexp @ irest in
      (iall, env_rest, next_stack_index)

let compile (p : pos expr) : instruction list = compile_env p 1 []

(* Start at the first stack slot, with an empty environment *)

(* The entry point for the compiler: a function that takes a expr and
   creates an assembly-program string representing the compiled version *)
let compile_to_string (prog : pos expr) : string =
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let asm_string = to_asm_string (compile prog @ [IRet]) in
  let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program
