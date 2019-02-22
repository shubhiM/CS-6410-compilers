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
let const_is_tail = false

let error_not_number = "error_not_number";;
let error_not_boolean = "error_not_boolean";;

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
  let rec find_in_env (env : (string * sourcespan) list) (key : string) : (string * sourcespan) option =
    match env with
    | [] -> None
    | (x, p) as b::xs -> if (compare x key) == 0 then Some(b) else (find_in_env xs key)
  in
  let rec wf_E (e : 'a expr) (global_env : 'a decl list) (local_env : (string * sourcespan) list) : exn list =
      match e with
       | ENumber(n, pos) ->
        if (n > const_max_int || n < const_min_int)
        then
        [Overflow(n, pos)]
        else []
       | EBool(b, pos) -> []
       | EId(id, pos) ->
          let id_in_local = (find_in_env local_env id) in
          (match id_in_local with
            | None ->
              [UnboundId(id, pos)]
            | _ ->[]
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
        let (body_env, binding_exns, named_exprs_exns, shadow_exns) = List.fold_left
        (* bind_env is the accumulator for collecting first appearance of a new binding *)
        (* dup_b_errs is accumulator for collecting errors associated with the duplicate bindings *)
        (* named_exp_errs is accumulator for collecting errors associated with the named expressions *)
        (* shadow_errs is accumulator for collecting errors associated with the shadow names *)
        (fun ((bind_env : (string * sourcespan) list),
              (dup_b_errs : exn list),
              (named_exp_errs : exn list),
              (shadow_errs : exn list)
              )
            ((x, exp, p) : sourcespan bind) ->

              let x_in_bind_env = (find_in_env bind_env x) in
              let x_in_local_env = (find_in_env local_env x) in

              (* current bind_env need to go into the local env to get semantics the let* form *)
              let exp_errs = (wf_E exp global_env (bind_env @local_env)) in
              let (env, b_err, n_err, s_err) = match x_in_bind_env with
                                                | None ->
                                                  (bind_env @ [(x, p)], dup_b_errs, named_exp_errs @ exp_errs, shadow_errs)
                                                | Some((x_in_bind, x_in_bind_pos)) ->

                                                  (bind_env, dup_b_errs @ [DuplicateId(x, p, x_in_bind_pos)],
                                                  named_exp_errs @ exp_errs, shadow_errs)

              in
              match x_in_local_env with
              | None ->
                (env, b_err, n_err, s_err)
              | Some((x_in_local, x_in_local_pos)) ->
                (env, b_err, n_err, s_err @ [ShadowId(x, p, x_in_local_pos)])
        )
        ([], [], [], [])
        bindings
        in
        (*NOTE: body_env contains either the complete bindings of let or one instance per binding *)
        (* this information is enough to reason about well formedness of the body expression in let *)
        let body_exns = (wf_E body global_env (body_env @ local_env)) in
        binding_exns @ named_exprs_exns @ shadow_exns @ body_exns
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
  and wf_D (d : 'a decl) (global_env : 'a decl list) : exn list =
     match d with
     | DFun(funname, args, body, pos_u) ->
        let fun_name_err = (match (find_decl global_env funname) with
         | Some(DFun(_, _, _, pos_d)) -> [DuplicateFun(funname, pos_u, pos_d)]
         | None -> [])
        in
        let (duplicate_args_errs, body_env) = List.fold_left
        (fun
          ((errs : exn list), (arg_env : (string * sourcespan) list))
          ((arg, pos) : string * sourcespan)
          ->
            let arg_in_env = (find_in_env arg_env arg) in
            match arg_in_env with
              | None -> (errs, arg_env @ [(arg, pos)])
              | Some((a, p)) -> (errs @ [DuplicateId(arg, pos, p)], arg_env)
        )
        ([], [])
        args
        in
        (* Adding this function name in the global environment that we pass to the function body
         to allow for recursive function constructs
         *)
        let body_errs = (wf_E body (d::global_env) body_env)
        in
        fun_name_err @ duplicate_args_errs @ body_errs
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


let rec compile_fun (fun_name : string) args env is_tail : instruction list =
   let push = List.map (fun (arg : tag immexpr) -> IPush(compile_imm arg env false)) (List.rev args) in
   let call = [ICall fun_name] in
   let clean = [IAdd (Reg ESP, Const(4 * (List.length args)))] in
   push @ call @ clean
and compile_aexpr (e : tag aexpr) (si : int) (env : arg envt) (num_args : int) (is_tail : bool) : instruction list =
  match e with
  | ALet(id, id_exp, body, tag) ->
    (* Let bindings are never in tail position. *)
    let compiled_named_expr = (compile_cexpr id_exp (si + 1) env num_args false) in

    (* printf "compiling named exp %s\n" (string_of_cexpr id_exp);
    printf "compiled instructions for named expr \n%s\n" (to_asm compiled_named_expr); *)

    let compiled_body = (compile_aexpr body (si + 1) ((id, Sized(DWORD_PTR, RegOffset(~-si, EBP)))::env) num_args is_tail) in

    (* let a = [IMov(RegOffset(~-si, EBP), Reg(EAX))] in *)
    (* printf "store result of named_exp on stack \n%s\n" (to_asm a); *)

    compiled_named_expr @ [IMov(RegOffset(~-si, EBP), Reg(EAX))] @ compiled_body
  | ACExpr(cexp) ->
     (compile_cexpr cexp si env num_args is_tail)
and compile_cexpr (e : tag cexpr) si env num_args is_tail =
  let assert_arith_num = [ITest (Reg(EAX), Const(1)); IJne ("err_ARITH_NOT_NUM")] in
  let assert_cmp_num = [ITest (Reg(EAX), Const(1)); IJne ("err_COMP_NOT_NUM")] in
  let assert_logic_not_bool = [ITest (Reg(EAX), Const(1)); IJe ("err_LOGIC_NOT_BOOL")] in
  let assert_if_not_bool = [ITest (Reg(EAX), Const(1)); IJe ("err_IF_NOT_BOOL")] in
  (* TODO: add assertions to check for integer overflow conditions *)
  let assert_overflow = [] in
  (* helper function to compile the type predicates like is_bool and is_num *)
  let compile_type_predicates (typ : string) (tag : int) : instruction list =
    let number_label = sprintf "isnumber_%d" tag in
    let done_label = sprintf "done_%d" tag in
    let prelude = [ITest (Reg EAX, Const(1)); IJe number_label] in
    let type_ins = match typ with
       | "bool" -> [IMov (Reg EAX, const_true); IJmp done_label;
                    ILabel number_label; IMov (Reg EAX, const_false);
                    ILabel done_label]
       | "number" -> [IMov (Reg EAX, const_false); IJmp done_label;
                      ILabel number_label; IMov (Reg EAX, const_true);
                      ILabel done_label]
       | _ -> failwith "Unsupported type"
     in
     prelude @ type_ins
  in
  let compile_bin_ops (v1 : arg) (v2 : arg) (si : int) (asserts : instruction list) (ops : instruction list) : instruction list =
    [IMov (Reg EAX, v1)]
    @ asserts
    @ [IMov (RegOffset(~-si, EBP), Reg EAX); IMov (Reg EAX, v2)]
    @ asserts
    @ [IMov (Reg EAX, RegOffset(~-si, EBP))]
    @ ops
  in
  let compile_cmp_expr (op : string) (si : int) (tag : int) (l : arg) (r : arg) : instruction list =
    let op_label = sprintf "%s_%d" op tag in
    let done_label = sprintf "done_%d" tag in
    let prelude =  [IMov (Reg EAX, l)] @
                    assert_cmp_num @
                    [IMov (RegOffset(~-si, EBP), Reg EAX); IMov (Reg EAX, r)] @
                    assert_cmp_num @
                    [IMov (Reg EAX, RegOffset(~-si, EBP))] @
                    [ICmp (Reg EAX, r); IMov (Reg EAX, const_true)]
    in
    let suffix = [IMov (Reg EAX, const_false);
                  IJmp done_label;
                  ILabel op_label;
                  ILabel done_label
                  ]
    in
    let body = match op with
                 | "greater" -> [IJg op_label]
                 | "greater_eq" -> [IJge op_label]
                 | "less" -> [IJl op_label]
                 | "less_eq" -> [IJle op_label]
                 | "eq" -> [IJe op_label]
                 | _ -> failwith "Unsupported comparison operator used"
    in
    prelude @ body @ suffix
  in
  match e with
  | CImmExpr(imm) -> [IMov(Reg(EAX), compile_imm imm env is_tail)]
  | CPrim1(op, e, tag) ->
    let compiled_e = [IMov(Reg(EAX), compile_imm e env false)] in
    (match op with
      | Add1 -> compiled_e @ assert_arith_num @ [IAdd (Reg EAX, Const 2)]
      | Sub1 -> compiled_e @ assert_arith_num @ [ISub (Reg EAX, Const 2)]
      | Not -> compiled_e @ assert_logic_not_bool @ [IXor (Reg EAX, bool_mask)]
      | IsBool -> compiled_e @ compile_type_predicates "bool" tag
      | IsNum  -> compiled_e @ compile_type_predicates "number" tag
      | Print -> failwith "Print is not implemented yet"
      | PrintStack -> failwith "PrintStack is not implemented yet"
      | _ -> failwith "Illegal expression!")
  | CPrim2(op, e1, e2, tag) ->
    let v1 = compile_imm e1 env false in
    let v2 = compile_imm e2 env false in
    (match op with
      | Plus ->
        (compile_bin_ops v1 v2 si assert_arith_num [IAdd(Reg EAX, v2)])
      | Minus ->
        (compile_bin_ops v1 v2 si assert_arith_num [ISub(Reg EAX, v2)])
      | Times ->
        (compile_bin_ops v1 v2 si assert_arith_num [IMul(Reg EAX, v2); ISar(Reg EAX, Const(1))])
      | And ->
        (compile_bin_ops v1 v2 si assert_logic_not_bool [IAnd(Reg(EAX), v2)])
      | Or ->
        (compile_bin_ops v1 v2 si assert_logic_not_bool [IOr(Reg EAX, v2)])
      | Greater ->
        (compile_cmp_expr "greater" si tag v1 v2)
      | GreaterEq ->
        (compile_cmp_expr "greater_eq" si tag v1 v2)
      | Less ->
        (compile_cmp_expr "less" si tag v1 v2)
      | LessEq ->
        (compile_cmp_expr "less_eq" si tag v1 v2)
      | Eq ->
        (compile_cmp_expr "eq" si tag v1 v2)
      | _ -> failwith "Illegal expression!")
  | CIf(cond, thn, els, tag) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
       [IMov(Reg(EAX), compile_imm cond env false)]
       @ assert_if_not_bool
       @ [ICmp (Reg EAX, const_false); IJe else_label]
       @ (compile_aexpr thn si env num_args is_tail)
       @ [IJmp done_label; ILabel else_label]
       @ (compile_aexpr els si env num_args is_tail)
       @ [ILabel done_label]
  | CApp(funname, args, tag) ->
    (compile_fun funname args env is_tail)
and compile_imm e env is_tail =
  match e with
  | ImmNum(n, _) -> Const((n lsl 1))
  | ImmBool(true, _) -> const_true
  | ImmBool(false, _) -> const_false
  | ImmId(x, _) -> (find env x)

let compile_decl (d : tag adecl) : instruction list =
 (* Callee duties *)
 (*
   1. Store Caller's EBP on stack
   2. Mov ebp, esp
   3. reserve space for local variables
   4. Add function arguments to the body environment
   5. compile the body
   6. restore esp to its old value from ebp
   7. restore ebp from stack to caller's ebp
   8. return
  *)
  match d with
  | ADFun(funname, args, body, tag) ->
         let n_locals = (count_vars body) in
         let stack_set_up = [
          ILabel(funname);
          ILineComment("-----stack setup-----");
          IPush (Reg(EBP));
          IMov (Reg(EBP), Reg(ESP));
          (* 16 byte allignment for mac os *)
          ISub(Reg(ESP), Const((4*n_locals/16+1)*16));
          ILineComment("-----compiled code-----");
        ]
        in
        let stack_clean_up = [
          ILineComment("-----stack clean up-----");
          IMov(Reg(ESP), Reg(EBP));
          IPop(Reg(EBP));
          IRet;
        ]
        in
        let num_args = List.length args in
        (* we know how caller must have pushed the arguments on the stack, we use
        that knowledge to prepare the environment for body to execute *)
        let (env, si_arg) = List.fold_left
        (fun ((env : arg envt), (si_arg : int)) (arg : string) ->
             let new_binding_in_env = (arg, Sized(DWORD_PTR, RegOffset(si_arg, EBP))) in
             (new_binding_in_env::env, si_arg + 1)
        )
        ([] , 2)
        (* There are two things between the first arg pushed by caller*)
        (* EBP currently points to the new call stack *)
        args
        in
        let si_local = 1 in
        (* body is the last thing that we compile and hence its in the tail position. *)
        let body_ins = (compile_aexpr body si_local env num_args true) in
        stack_set_up @ body_ins @ stack_clean_up

let compile_prog (anfed : tag aprogram) : string =
  let prelude =
    "section .text
     extern error
     extern print"
  in
  match anfed with
    | AProgram(funs, main, tag) ->
        (* treating the body of the program as the main entrypoint function *)
        (* printf "Program -  %s\n" (string_of_aprogram anfed); *)
        let main_fun = ADFun("our_code_starts_here", [], main, tag) in
        let all_decls = funs @ [main_fun] in
        let compiled_decls = List.fold_left
        (fun (compiled_exp : string) (ADFun(funname, args, body, tag) as d : tag adecl) ->
              let prelude = "global " ^ funname in
              compiled_exp ^ prelude ^ (to_asm (compile_decl d)) ^ "\n"
        )
        ""
        all_decls
        in
        (* let err_COMP_NOT_NUM   = 0
        let err_ARITH_NOT_NUM  = 1
        let err_LOGIC_NOT_BOOL = 2
        let err_IF_NOT_BOOL    = 3
        let err_OVERFLOW       = 4 *)
        let postlude = [
            ILineComment("-----errors-----");
            ILabel ("err_ARITH_NOT_NUM");
            IPush (Reg(EAX));
            IPush (Const(err_ARITH_NOT_NUM));
            ICall "error";
            IAdd (Reg(ESP), Const(8));

            ILabel ("err_LOGIC_NOT_BOOL");
            IPush (Reg(EAX));
            IPush (Const(err_LOGIC_NOT_BOOL));
            ICall "error";
            IAdd (Reg(ESP), Const(8));

            ILabel ("err_IF_NOT_BOOL");
            IPush (Reg(EAX));
            IPush (Const(err_IF_NOT_BOOL));
            ICall "error";
            IAdd (Reg(ESP), Const(8));

            ILabel ("err_COMP_NOT_NUM");
            IPush (Reg(EAX));
            IPush (Const(err_COMP_NOT_NUM));
            ICall "error";
            IAdd (Reg(ESP), Const(8));

            ILabel ("err_OVERFLOW");
            IPush (Reg(EAX));
            IPush (Const(err_OVERFLOW));
            ICall "error";
            IAdd (Reg(ESP), Const(8));
        ]
        in
        sprintf "%s\n%s\n%s\n" prelude compiled_decls (to_asm postlude)

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
