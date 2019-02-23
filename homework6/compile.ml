open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors

(* Add at least one of these two *)
(* open TypeCheck *)
(* open Inference *)

type 'a envt = (string * 'a) list

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (binds, body, _) ->
      List.for_all (fun (_, e, _) -> is_anf e) binds && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e

and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false

let const_true = HexConst 0xFFFFFFFF

let const_false = HexConst 0x7FFFFFFF

let bool_mask = HexConst 0x80000000

let tag_as_bool = HexConst 0x00000001

let err_COMP_NOT_NUM = 0

let err_ARITH_NOT_NUM = 1

let err_LOGIC_NOT_BOOL = 2

let err_IF_NOT_BOOL = 3

let err_OVERFLOW = 4

let const_max_int = 1073741823

let const_min_int = -1073741824

(* You may find some of these helpers useful *)
let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y, v) :: rest -> if y = x then v else find rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ACExpr e -> helpC e
  and helpC e =
    match e with CIf (_, t, f, _) -> max (helpA t) (helpA f) | _ -> 0
  in
  helpA e

let rec replicate x i = if i = 0 then [] else x :: replicate x (i - 1)

let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
  | [] -> None
  | (DFun (fname, _, _, _, _) as d) :: ds_rest ->
      if name = fname then Some d else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with [] -> false | x :: xs -> elt = x || find_one xs elt

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | [x] -> None
  | x :: xs -> if find_one xs x then Some x else find_dup xs

(* IMPLEMENT EVERYTHING BELOW *)

let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with Program (decls, body, typ, tag) ->
      Program
        ( List.map (fun g -> List.map (helpD env) g) decls
        , helpE env body
        , typ
        , tag )
  and helpD env decl =
    match decl with DFun (name, args, scheme, body, tag) ->
      let newArgs =
        List.map (fun (a, tag) -> (a, sprintf "%s#%d" a tag)) args
      in
      let env' = newArgs @ env in
      DFun
        ( name
        , List.map2 (fun (a, a') (_, tag) -> (a', tag)) newArgs args
        , scheme
        , helpE env' body
        , tag )
  and helpE env e =
    match e with
    | EAnnot (e, t, tag) -> helpE env e
    | EPrim1 (op, arg, tag) -> EPrim1 (op, helpE env arg, tag)
    | EPrim2 (op, left, right, tag) ->
        EPrim2 (op, helpE env left, helpE env right, tag)
    | EIf (c, t, f, tag) -> EIf (helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | EId (name, tag) -> EId (find env name, tag)
    | EApp (name, args, tag) -> EApp (name, List.map (helpE env) args, tag)
    | ELet (binds, body, tag) ->
        let rev_binds, env' =
          List.fold_left
            (fun (rev_binds, env) ((name, typ, tag1), expr, tag2) ->
              let name' = sprintf "%s#%d" name tag1 in
              let expr' = helpE env expr in
              let env' = (name, name') :: env in
              (((name', typ, tag1), expr', tag2) :: rev_binds, env') )
            ([], env) binds
        in
        let body' = helpE env' body in
        ELet (List.rev rev_binds, body', tag)
  in
  rename [] p

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with Program (decls, body, typ, _) ->
      AProgram (List.concat (List.map helpG decls), helpA body, ())
  and helpG (g : tag decl list) : unit adecl list = List.map helpD g
  and helpD (d : tag decl) : unit adecl =
    match d with DFun (name, args, ret, body, _) ->
      ADFun (name, List.map fst args, helpA body, ())
  and helpC (e : tag expr) : unit cexpr * (string * unit cexpr) list =
    match e with
    | EAnnot (e, _, _) -> helpC e
    | EPrim1 (op, arg, _) ->
        let arg_imm, arg_setup = helpI arg in
        (CPrim1 (op, arg_imm, ()), arg_setup)
    | EPrim2 (op, left, right, _) ->
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf (cond, _then, _else, _) ->
        let cond_imm, cond_setup = helpI cond in
        (CIf (cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet ([], body, _) -> helpC body
    | ELet (((bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EApp (funname, args, _) ->
        let new_args, new_setup = List.split (List.map helpI args) in
        (CApp (funname, new_args, ()), List.concat new_setup)
    | _ ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
  and helpI (e : tag expr) : unit immexpr * (string * unit cexpr) list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
    | EAnnot (e, _, _) -> helpI e
    | EPrim1 (op, arg, tag) ->
        let tmp = sprintf "unary_%d" tag in
        let arg_imm, arg_setup = helpI arg in
        (ImmId (tmp, ()), arg_setup @ [(tmp, CPrim1 (op, arg_imm, ()))])
    | EPrim2 (op, left, right, tag) ->
        let tmp = sprintf "binop_%d" tag in
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        ( ImmId (tmp, ())
        , left_setup @ right_setup
          @ [(tmp, CPrim2 (op, left_imm, right_imm, ()))] )
    | EIf (cond, _then, _else, tag) ->
        let tmp = sprintf "if_%d" tag in
        let cond_imm, cond_setup = helpI cond in
        ( ImmId (tmp, ())
        , cond_setup @ [(tmp, CIf (cond_imm, helpA _then, helpA _else, ()))] )
    | EApp (funname, args, tag) ->
        let tmp = sprintf "app_%d" tag in
        let new_args, new_setup = List.split (List.map helpI args) in
        ( ImmId (tmp, ())
        , List.concat new_setup @ [(tmp, CApp (funname, new_args, ()))] )
    | ELet ([], body, _) -> helpI body
    | ELet (((bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right
      (fun (bind, exp) body -> ALet (bind, exp, body, ()))
      ans_setup (ACExpr ans)
  in
  helpP p

let is_well_formed (p : sourcespan program) : sourcespan program fallible =
  let rec find_in_env env key =
    match env with
    | [] -> None
    | ((x, p) as b) :: xs ->
        if compare x key == 0 then Some b else find_in_env xs key
  in
  let rec wf_E e global_env local_env =
    match e with
    | ENumber (n, pos) ->
        if n > const_max_int || n < const_min_int then [Overflow (n, pos)]
        else []
    | EBool (b, pos) -> []
    | EId (id, pos) -> (
        let id_in_local = find_in_env local_env id in
        match id_in_local with None -> [UnboundId (id, pos)] | _ -> [] )
    | EIf (cond, thn, els, pos) ->
        let cond_err = wf_E cond global_env local_env in
        let then_err = wf_E thn global_env local_env in
        let else_err = wf_E els global_env local_env in
        cond_err @ then_err @ else_err
    | EPrim1 (prim1, e1, pos) -> wf_E e1 global_env local_env
    | EPrim2 (prim2, e1, e2, pos) ->
        let e1_err = wf_E e1 global_env local_env in
        let e2_err = wf_E e2 global_env local_env in
        e1_err @ e2_err
    | ELet (bindings, body, pos) ->
        let body_env, binding_exns, named_exprs_exns, shadow_exns =
          List.fold_left
            (* bind_env is the accumulator for collecting first appearance of a new binding *)
            (* dup_b_errs is accumulator for collecting errors associated with the duplicate bindings *)
            (* named_exp_errs is accumulator for collecting errors associated with the named expressions *)
            (* shadow_errs is accumulator for collecting errors associated with the shadow names *)
              (fun ( (bind_env : (string * sourcespan) list)
                   , (dup_b_errs : exn list)
                   , (named_exp_errs : exn list)
                   , (shadow_errs : exn list) )
                   (((x, typ, _), exp, p) : sourcespan bind) ->
              let x_in_bind_env = find_in_env bind_env x in
              let x_in_local_env = find_in_env local_env x in
              (* current bind_env need to go into the local env to get semantics the let* form *)
              let exp_errs = wf_E exp global_env (bind_env @ local_env) in
              let env, b_err, n_err, s_err =
                match x_in_bind_env with
                | None ->
                    ( bind_env @ [(x, p)]
                    , dup_b_errs
                    , named_exp_errs @ exp_errs
                    , shadow_errs )
                | Some (x_in_bind, x_in_bind_pos) ->
                    ( bind_env
                    , dup_b_errs @ [DuplicateId (x, p, x_in_bind_pos)]
                    , named_exp_errs @ exp_errs
                    , shadow_errs )
              in
              match x_in_local_env with
              | None -> (env, b_err, n_err, s_err)
              | Some (x_in_local, x_in_local_pos) ->
                  (env, b_err, n_err, s_err @ [ShadowId (x, p, x_in_local_pos)])
              )
            ([], [], [], []) bindings
        in
        (*NOTE: body_env contains either the complete bindings of let or one instance per binding *)
        (* this information is enough to reason about well formedness of the body expression in let *)
        let body_exns = wf_E body global_env (body_env @ local_env) in
        binding_exns @ named_exprs_exns @ shadow_exns @ body_exns
    | EApp (funname, args, pos) ->
        let f_decl = find_decl global_env funname in
        let fun_errs =
          match f_decl with
          | None -> [UnboundFun (funname, pos)]
          | Some (DFun (fname, fargs, scheme, fbody, fp)) ->
              let actual_arity = List.length args in
              let intended_arity = List.length fargs in
              if actual_arity != intended_arity then
                [Arity (intended_arity, actual_arity, pos)]
              else []
        in
        let arg_errs =
          List.fold_left
            (fun (err : exn list) (arg : 'a expr) ->
              let arg_err = wf_E arg global_env local_env in
              err @ arg_err )
            [] args
        in
        fun_errs @ arg_errs
    | EAnnot (_, _, _) ->
        failwith "EAnnot expressions are not part of well formednes checks"
  and wf_D d global_env =
    match d with DFun (funname, args, scheme, body, pos_u) ->
      let duplicate_args_errs, body_env =
        List.fold_left
          (fun ((errs : exn list), (arg_env : (string * sourcespan) list))
               ((arg, pos) : string * sourcespan) ->
            let arg_in_env = find_in_env arg_env arg in
            match arg_in_env with
            | None -> (errs, arg_env @ [(arg, pos)])
            | Some (a, p) -> (errs @ [DuplicateId (arg, pos, p)], arg_env) )
          ([], []) args
      in
      let body_errs = wf_E body global_env body_env in
      duplicate_args_errs @ body_errs
  and wf_G g env =
    (* decl environment has all the decls from previously defined groups and the group it belongs to itself to allow for forward reference within a group *)
    let decl_env = env @ g in
    let duplicate_fun_errors, new_env =
      List.fold_left
        (fun ((errs : exn list), (env : 'a decl list)) (d : 'a decl) ->
          match d with DFun (name, args, scheme, body, pos_u) -> (
            match find_decl env name with
            | Some (DFun (_, _, _, _, pos_d)) ->
                (DuplicateFun (name, pos_u, pos_d) :: errs, env)
            | None -> (errs, d :: env) ) )
        ([], []) decl_env
    in
    let decl_errs =
      List.fold_left
        (fun (errs : exn list) (d : 'a decl) -> errs @ wf_D d new_env)
        [] g
    in
    duplicate_fun_errors @ decl_errs
  in
  match p with Program (decl_groups, body, typ, _) -> (
    let decl_exns, global_env =
      List.fold_left
        (fun ((errs : exn list), (env : 'a decl list))
             (decl_group : 'a decl list) ->
          (* Note even adding the wrong decl to env does not impact evaluation since we always find the first
            defined decl in case of the duplicate decls
          *)
          (* the group is available for forward references to the next group *)
          (errs @ wf_G decl_group env, env @ decl_group) )
        ([], []) decl_groups
    in
    let body_err = wf_E body global_env [] in
    let all_err = decl_exns @ body_err in
    match all_err with [] -> Ok p | _ -> Error all_err )

let rec compile_fun (fun_name : string) args env : instruction list =
  raise (NotYetImplemented "Compile funs not yet implemented")

and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int)
    (is_tail : bool) : instruction list =
  raise (NotYetImplemented "Compile aexpr not yet implemented")

and compile_cexpr (e : tag cexpr) si env num_args is_tail =
  raise (NotYetImplemented "Compile cexpr not yet implemented")

and compile_imm e env =
  match e with
  | ImmNum (n, _) -> Const (n lsl 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x

let compile_decl (d : tag adecl) : instruction list =
  raise (NotYetImplemented "Compile decl not yet implemented")

let compile_prog (anfed : tag aprogram) : string =
  raise (NotYetImplemented "Compiling programs not implemented yet")

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)

(* Add a typechecking phase somewhere in here! *)
let compile_to_string (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_err_phase well_formed is_well_formed
  |> add_phase tagged tag
  |> add_phase renamed rename_and_tag
  (* this is the new phase that has been added to the pipeline to do remove some of the bugs that persisted in previous programs*)
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase result compile_prog
