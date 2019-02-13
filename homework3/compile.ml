open Printf
open Types
open Pretty

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (binds, body, _) ->
      List.for_all (fun (_, e, _) -> is_anf e) binds && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e

and is_imm e = match e with ENumber _ -> true | EId _ -> true | _ -> false

(* This function should encapsulate the binding-error checking from Adder *)
exception BindingError of string

let rec check_scope (e : (Lexing.position * Lexing.position) expr) : unit =
  failwith "check_scope: Implement this"

type tag = int

(* This function assigns a unique tag to every subexpression and let binding *)
let tag (e : 'a expr) : tag expr =
  let rec tag_helper (e : 'a expr) (curr : tag) =
    match e with
    | EId (x, _) -> (EId (x, curr), curr + 1)
    | ENumber (n, _) -> (ENumber (n, curr), curr + 1)
    | EPrim1 (op, e, _) ->
        let e_tagged, next_tag = tag_helper e (curr + 1) in
        (EPrim1 (op, e_tagged, curr), next_tag)
    | EPrim2 (op, e1, e2, _) ->
        let e1_tagged, next_tag = tag_helper e1 (curr + 1) in
        let e2_tagged, last_tag = tag_helper e2 next_tag in
        (EPrim2 (op, e1_tagged, e2_tagged, curr), last_tag)
    | ELet (binds, body, _) ->
        (*  Check if using a cons way of concating list gives better results.*)
        let tagged_binds, next_tag =
          List.fold_left
            (fun ((t_bindings : tag bind list), (n_tag : tag))
                 ((v, exp, _) : 'a bind) ->
              let t_exp, n_t = tag_helper exp (n_tag + 1) in
              (t_bindings @ [(v, t_exp, n_tag)], n_t) )
            ([], curr + 1)
            binds
        in
        let tagged_body, last_tag = tag_helper body next_tag in
        (ELet (tagged_binds, tagged_body, curr), last_tag)
    | EIf (cond, thn, els, _) ->
        let tagged_cond, next_tag_1 = tag_helper cond (curr + 1) in
        let tagged_thn, next_tag_2 = tag_helper thn next_tag_1 in
        let tagged_els, last_tag = tag_helper els next_tag_2 in
        (EIf (tagged_cond, tagged_thn, tagged_els, curr), last_tag)
  in
  let tagged, _ = tag_helper e 1 in
  tagged

(* This function removes all tags, and replaces them with the unit value.
   This might be convenient for testing, when you don't care about the tag info. *)
let rec untag (e : 'a expr) : unit expr =
  match e with
  | EId (x, _) -> EId (x, ())
  | ENumber (n, _) -> ENumber (n, ())
  | EPrim1 (op, e, _) -> EPrim1 (op, untag e, ())
  | EPrim2 (op, e1, e2, _) -> EPrim2 (op, untag e1, untag e2, ())
  | ELet (binds, body, _) ->
      ELet (List.map (fun (x, b, _) -> (x, untag b, ())) binds, untag body, ())
  | EIf (cond, thn, els, _) -> EIf (untag cond, untag thn, untag els, ())

let rec anf_helper (e : tag expr) : unit expr * unit bind list =
  match e with
  | ENumber (n, tag) ->
      let temp = sprintf "$enumber_%d" tag in
      (EId (temp, ()), [ (temp, ENumber(n, ()), ())])
  | EId (x, tag) ->
    let temp = sprintf "$eid_%d" tag in
    (EId(temp, ()) , [ (temp, EId(x , ()), ()) ])
  | EPrim1 (op, e, tag) ->
      let e_ans, e_context = anf_helper e in
      let temp = sprintf "$prim1_%d" tag in
      (EId (temp, ()), e_context @ [(temp, EPrim1 (op, e_ans, ()), ())])
  | EPrim2 (op, left, right, tag) ->
      let left_ans, left_context = anf_helper left in
      let right_ans, right_context = anf_helper right in
      let temp = sprintf "$prim2_%d" tag in
      ( EId (temp, ())
      , left_context @ right_context
        @ [(temp, EPrim2 (op, left_ans, right_ans, ()), ())])
  | ELet (bindings, body, tag) ->
      (* see if you need to use a cons way of concatenating lists because of any better reasons *)
      let anfed_bindings, anfed_binding_link, context_of_anfed_bindings =
        List.fold_left
          (fun ( (a_binds : unit bind list), (bind_links : (string * unit expr) list), (context : (string * unit expr) list) )
               ((v, exp, tg) : tag bind) ->
            let b_ans, b_contxt = anf_helper exp in
            (a_binds @ [(v, b_ans, ())], bind_links @ [(v, b_ans)], context @ b_contxt)
            )
          ([], [], []) bindings
      in
      let anfed_body, anfed_body_context = anf_helper body in
      let temp = sprintf "$let_%d" tag in
      ( EId (temp, ())
      , context_of_anfed_bindings
        @ anfed_binding_link
        @ anfed_body_context
        @ [(temp, ELet (anfed_bindings, anfed_body, ()))] )
  | EIf (cond, thn, els, tag) ->
      let cond_ans, cond_context = anf_helper cond in
      let thn_ans, thn_context = anf_helper thn in
      let els_ans, els_context = anf_helper els in
      let temp = sprintf "if_%d" tag in
      ( EId (temp, ())
      , cond_context @ thn_context @ els_context
        @ [(temp, EIf (cond_ans, thn_ans, els_ans, ()))] )


let rec create_nested_let_exprs
  (bindings : (string * unit expr) list)
  (body : unit expr) :
    unit expr =
      (match bindings with
      | [] -> body
      | ((x, e)::rest) ->
          let transformed_e = (create_nested_let_expr e) in
          ELet(
             [(x, transformed_e, ())],
             (create_nested_let_exprs rest body), ()))
and create_nested_let_expr (ex : unit expr) : unit expr =
  match ex with
  | ELet(binds, bd, _) -> (create_nested_let_exprs binds bd)
  | _ -> ex

(* This function converts a tagged expression into an untagged expression in A-normal form *)
let anf (e : tag expr) : unit expr =
  (* what if you have the context of the same type as a unit bind  list *)
  let e_ans, e_context = anf_helper e
  in create_nested_let_exprs e_context e_ans

(* Helper functions *)
let r_to_asm (r : reg) : string = match r with EAX -> "eax" | ESP -> "esp"

let arg_to_asm (a : arg) : string =
  match a with
  | Const n -> sprintf "%d" n
  | Reg r -> r_to_asm r
  | RegOffset (n, r) ->
      if n >= 0 then sprintf "[%s+%d]" (r_to_asm r) (word_size * n)
      else sprintf "[%s-%d]" (r_to_asm r) (-1 * word_size * n)

let i_to_asm (i : instruction) : string =
  match i with
  | IMov (dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd (dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | IRet -> "  ret"
  | ISub (dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul (dest, to_mul) ->
      sprintf "  mul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | ILabel label -> sprintf "  %s:" label
  | ICmp (dest, to_cmp) ->
      sprintf "  cmp %s, %s" (arg_to_asm dest) (arg_to_asm to_cmp)
  | IJne label -> sprintf "  jne %s" label
  | IJe label -> sprintf "  je %s" label
  | IJmp label -> sprintf "  jmp %s" label

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let rec find ls x =
  match ls with
  | [] -> failwith (sprintf "Name %s not found" x)
  | (y, v) :: rest -> if y = x then v else find rest x

(* This function actually compiles the tagged ANF expression into assembly *)
(* The si parameter should be used to indicate the next available stack index for use by Lets *)
(* The env parameter associates identifier names to stack indices *)
let rec compile_expr (e : tag expr) (si : int) (env : (string * int) list) :
    instruction list =
  match e with
  | ENumber (n, _) -> [IMov (Reg EAX, compile_imm e env)]
  | EId (x, _) -> [IMov (Reg EAX, compile_imm e env)]
  | EPrim1 (op, e, _) -> (
      let inst_1 = compile_expr e si env in
      match op with
      | Add1 -> inst_1 @ [IAdd (Reg EAX, Const 1)]
      | Sub1 -> inst_1 @ [ISub (Reg EAX, Const 1)] )
  | EPrim2 (op, left, right, _) -> (
      let left_value = compile_imm left env in
      let right_value = compile_imm right env in
      match op with
      | Plus -> [IMov (Reg EAX, left_value); IAdd (Reg EAX, right_value)]
      | Minus -> [IMov (Reg EAX, left_value); ISub (Reg EAX, right_value)]
      | Times -> [IMov (Reg EAX, left_value); IMul (Reg EAX, right_value)] )
  | EIf (cond, thn, els, tag) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      compile_expr cond si env
      (* this is an immediate expression *)
      @ [ICmp (Reg EAX, Const 0); IJe else_label]
      @ compile_expr thn si env
      @ [IJmp done_label; ILabel else_label]
      @ compile_expr els si env @ [ILabel done_label]
  | ELet ([(id, e, _)], body, _) ->
      printf "I am here\n\n" ;
      (* Anfed let has only one binding at each level *)
      let is_id = compile_expr e (si + 1) env in
      is_id
      @ [IMov (RegOffset (~-si, ESP), Reg EAX)]
      @ compile_expr body (si + 1) ([(id, si)] @ env)
  | _ -> failwith "Impossible: Not in ANF"

and compile_imm e env =
  match e with
  | ENumber (n, _) -> Const n
  | EId (x, _) -> RegOffset (~-(find env x), ESP)
  | _ -> failwith "Impossible: not an immediate"

let compile_anf_to_string anfed =
  let prelude =
    "section .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  let compiled = compile_expr anfed 1 [] in
  (* compiling the anfed expression *)
  let as_assembly_string = to_asm (compiled @ [IRet]) in
  sprintf "%s%s\n" prelude as_assembly_string

let compile_to_string prog =
  (*check_scope prog;*)
  let tagged : tag expr = tag prog in
  (* first we are tagging the input program by the user *)
  let anfed : tag expr = tag (anf tagged) in
  (* converting tagged expression to its ANF form *)
  printf "Prog:\n%s\n" (ast_of_expr prog) ;
  printf "Tagged:\n%s\n" (format_expr tagged string_of_int) ;
  printf "ANFed/tagged:\n%s\n" (format_expr anfed string_of_int) ;
  compile_anf_to_string anfed
