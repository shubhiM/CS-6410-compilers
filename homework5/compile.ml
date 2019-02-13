open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors

type 'a envt = (string * 'a) list

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;


let const_true = HexConst (0xFFFFFFFF)
let const_false = HexConst(0x7FFFFFFF)
let bool_mask = HexConst(0x80000000)
let tag_as_bool = HexConst(0x00000001)

let err_COMP_NOT_NUM   = 0
let err_ARITH_NOT_NUM  = 1
let err_LOGIC_NOT_BOOL = 2
let err_IF_NOT_BOOL    = 3
let err_OVERFLOW       = 4
let const_max_int      = 1073741823
let const_min_int      = -1073741824


(* You may find some of these helpers useful *)
let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y,v)::rest ->
     if y = x then v else find rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ALet(_, bind, body, _) -> 1 + (max (helpC bind) (helpA body))
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf(_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in helpA e

let rec replicate x i =
  if i = 0 then []
  else x :: (replicate x (i - 1))


let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

(* helper that finds the binding in the list of bindings so called local env *)
let rec find_bind (bs : 'a bind list) (name : string) : 'a bind option =
  match bs with
    | [] -> None
    | ((bname, _, _) as b)::bs_rest ->
     if name = bname then Some(b) else find_bind bs_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs
;;

(* IMPLEMENT EVERYTHING BELOW *)
let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program(decls, body, _) -> AProgram(List.map helpD decls, helpA body, ())
  and helpD (d : tag decl) : unit adecl =
    match d with
    | DFun(name, args, body, _) -> ADFun(name, List.map fst args, helpA body, ()) (* why are we doing fst here *)
  and helpC (e : tag expr) : (unit cexpr * (string * unit cexpr) list) =
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (CPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (CPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (CIf(cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet([], body, _) -> helpC body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EApp(funname, args, _) ->
       let imm_args, args_setup = help_imm_args args in
       (CApp(funname, imm_args, ()), args_setup)
    | _ -> let (imm, setup) = helpI e in (CImmExpr imm, setup)
  and helpI (e : tag expr) : (unit immexpr * (string * unit cexpr) list) =
    match e with
    | ENumber(n, _) -> (ImmNum(n, ()), [])
    | EBool(b, _) -> (ImmBool(b, ()), [])
    | EId(name, _) -> (ImmId(name, ()), [])
    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (ImmId(tmp, ()), arg_setup @ [(tmp, CPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (ImmId(tmp, ()), left_setup @ right_setup @ [(tmp, CPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (ImmId(tmp, ()), cond_setup @ [(tmp, CIf(cond_imm, helpA _then, helpA _else, ()))])
    | EApp(funname, args, tag) ->
        let tmp = sprintf "app_%d" tag in
        let imm_args, args_setup = help_imm_args args in
       (ImmId(tmp, ()), args_setup @ [(tmp, CApp(funname, imm_args, ()))])
    | ELet([], body, _) -> helpI body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in (* Why do we make call to help c here if we want to stay in here*)
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
  and helpA e : unit aexpr =
    let (ans, ans_setup) = helpC e in
    List.fold_right (fun (bind, exp) body -> ALet(bind, exp, body, ())) ans_setup (ACExpr ans)
  and help_imm_args (args : tag expr list) : (unit immexpr list * (string * unit cexpr) list) =
    List.fold_left
    (fun ((ags : unit immexpr list), (setup : (string * unit cexpr) list)) (arg : tag expr) ->
      let imm_arg, arg_setup = helpI arg in
      (ags @ [imm_arg], setup @ arg_setup)
    )
    ([], [])
    args
  in
  helpP p
;;


let is_well_formed (p : sourcespan program) : (sourcespan program) fallible =
  let giant_let (bindings : 'a bind list) (body : 'a expr) (pos : sourcespan) : 'a expr =
    List.fold_right
    (fun (b : 'a bind) (body : 'a expr) ->
      ELet([b], body, pos)
    )
    bindings
    body
    in
  let rec wf_E (e : 'a expr) (global_env : 'a decl list) (local_env : 'a bind list) : exn list =
      match e with
       | ENumber(n, pos) ->
        if (n > const_max_int || n < const_min_int)
        then [Overflow(n, pos)]
        else []
       | EBool(b, pos) -> []
       | EId(id, pos) ->
          (match (find_bind local_env id) with
            | None -> []
            | _ -> [UnboundId(id, pos)]
          )
       | EIf(cond, thn, els, pos) ->
             let cond_err = (wf_E cond global_env local_env) in
             let then_err = (wf_E thn global_env local_env) in
             let else_err = (wf_E els global_env local_env) in
             cond_err @ then_err @ else_err
      | EPrim1(prim1, e1, pos) ->
        (wf_E e1 global_env local_env)
      | EPrim2(prim2, e1, e2, pos) ->
        let e1_err = (wf_E e1 global_env local_env) in
        let e2_err = (wf_E e2 global_env local_env) in
        e1_err @ e2_err
      | ELet(bindings, body, pos) ->
        let big_let = giant_let bindings body pos in
        (match big_let with
          | ELet([(v2, e2, p2) as b], body, pos) ->
              let dup_bind_err = (match (find_bind local_env v2) with
                                  | None -> []
                                  | Some((v1, e1, p1)) -> [DuplicateId(v1, p2, p1)])
              in
              let bind_expr_err =  (wf_E e2 global_env local_env) in
              let body_expr_err =  (wf_E body global_env (local_env @ [b])) in
              dup_bind_err @ bind_expr_err @ body_expr_err
          |_ ->
            let err_msg = "Expected a let with single binding but found " ^ (ast_of_expr e) in
            [InternalCompilerError(err_msg)])
      | EApp(funname, args, pos) ->
        let f_decl = (find_decl global_env funname) in
        let fun_errs = (match f_decl with
                          | None ->
                              [UnboundFun(funname, pos)]
                          | Some(DFun(fname, fargs, fbody, fp)) ->
                                let actual_arity = (List.length args) in
                                let intended_arity = (List.length fargs) in
                                if (actual_arity != intended_arity)
                                then [Arity(intended_arity,actual_arity, pos)]
                                else [])
       in
       let arg_errs = List.fold_left
       (fun (err : exn list) (arg : 'a expr) ->
          let arg_err = (wf_E arg global_env local_env) in
          err @ arg_err
       )
       []
       args
       in
       fun_errs @ arg_errs
  and wf_D (d : 'a decl) (env : 'a decl list) : exn list =
     match d with
     | DFun(funname, args, body, pos_u) ->
        let fun_name_err = (match (find_decl env funname) with
         | Some(DFun(_, _, _, pos_d)) -> [DuplicateFun(funname, pos_u, pos_d)]
         | None -> [])
        in
        let bind_err = (match (find_dup args) with
        | Some((b, pos_b)) -> [DuplicateId(b, pos_b, pos_u)]
        | None -> [])
        in
        let body_err = (wf_E body env [])
        in
        fun_name_err @ bind_err @ body_err
  in
  match p with
  | Program(decls, body, _) ->
     (* global environment is the list of declarations *)
     let (decl_exns, global_env) = List.fold_left
       (fun ((errs : exn list), (env : 'a decl list)) (d : 'a decl) ->
          (* Note even adding the wrong decl to env does not impact evaluation since we always find the first
             defined decl in case of the duplicate decls
           *)
          (errs @ (wf_D d env), env @ [d])
       )
       ([], [])
       decls
     in
     let body_err = (wf_E body global_env []) in
     let all_err = decl_exns @ body_err in
     match all_err with
     | [] -> Ok(p)
     | _ -> Error(all_err)
;;


let rec compile_fun (fun_name : string) args env : instruction list =
  raise (NotYetImplemented "Compile funs not yet implemented")
and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) : instruction list =
  raise (NotYetImplemented "Compile aexpr not yet implemented")
and compile_cexpr (e : tag cexpr) si env num_args is_tail =
  raise (NotYetImplemented "Compile cexpr not yet implemented")
and compile_imm e env =
  match e with
  | ImmNum(n, _) -> Const((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)

let compile_decl (d : tag adecl) : instruction list =
  raise (NotYetImplemented "Compile decl not yet implemented")

let compile_prog (anfed : tag aprogram) : string =
  raise (NotYetImplemented "Compiling programs not implemented yet")

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)

(*  sourcespan decorated program and program decorated pipeline *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> (add_err_phase well_formed is_well_formed)
  |> (add_phase tagged tag)
  |> (add_phase anfed (fun p -> atag (anf p)))
  |> (add_phase result compile_prog)
;;
