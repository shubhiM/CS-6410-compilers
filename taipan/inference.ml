open Exprs
open Errors
open Printf
open Pretty
open Phases

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

type 'a envt = 'a StringMap.t;;
type 'a subst = (string * 'a) list;;


let print_funenv funenv =
  StringMap.iter (fun name scheme -> debug_printf "\t%s => %s\n" name (string_of_scheme scheme)) funenv;;
let print_env env =
  StringMap.iter (fun name typ -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) env;;

let print_subst subst =
  List.iter (fun (name, typ) -> debug_printf "\t%s => %s\n" name (string_of_typ typ)) subst;;


let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)

let tInt = TyCon("Int", dummy_span)
let tBool = TyCon("Bool", dummy_span)
let tyVarX = TyVar("X", dummy_span)
let tyVarY = TyVar("Y", dummy_span)

let int2bool = SForall([], TyArr([tInt], tBool, dummy_span), dummy_span)
let int2int = SForall([], TyArr([tInt], tInt, dummy_span), dummy_span)
let bool2bool = SForall([], TyArr([tBool], tBool, dummy_span), dummy_span)
let any2bool = SForall(["X"], TyArr([tyVarX], tBool, dummy_span), dummy_span)
let any2any = SForall(["X"], TyArr([tyVarX], tyVarX, dummy_span), dummy_span)

let intint2int = SForall([], TyArr([tInt; tInt], tInt, dummy_span), dummy_span)
let boolbool2bool = SForall([], TyArr([tBool; tBool], tBool, dummy_span), dummy_span)
let intint2bool = SForall([], TyArr([tInt; tInt], tBool, dummy_span), dummy_span)

let xx2bool = SForall(["X"], TyArr([tyVarX; tyVarX], tBool, dummy_span), dummy_span)

(* create more type synonyms here, if you need to *)
let initial_env : sourcespan scheme envt =
  List.fold_left (fun env (name, typ) -> StringMap.add name typ env) StringMap.empty [
      (* unary ops *)
      ("Add1", int2int);
      ("Sub1", int2int);
      ("Print", any2any);
      ("IsNum", any2bool);
      ("IsBool", any2bool);
      ("Not", bool2bool);

      (* binary ops *)
      ("Plus", intint2int);
      ("Minus", intint2int);
      ("Times", intint2int);
      ("And", boolbool2bool);
      ("Or", boolbool2bool);
      ("Greater", intint2bool);
      ("Less", intint2bool);
      ("GreaterEq", intint2bool);
      ("LessEq", intint2bool);
      ("Eq", xx2bool);
  ]

let rec find_pos (ls : 'a envt) x pos : 'a =
  try
    StringMap.find x ls
  with
  | Not_found -> failwith (sprintf "Name %s not found at %s" x (string_of_sourcespan pos))
;;
let rec subst_var_typ ((tyvar, to_typ) as sub) in_typ =
  failwith "Implement substituting a type for a type variable, within a type, here"
;;
let subst_var_scheme ((tyvar, to_typ) as sub) scheme =
  failwith "Implement substituting a type for a type variable, within a scheme, here"
;;
let apply_subst_typ (subst : 'a typ subst) (t : 'a typ) : 'a typ =
  List.fold_left (fun t sub -> subst_var_typ sub t) t subst
;;
let apply_subst_scheme (subst : 'a typ subst) (scheme : 'a scheme) : 'a scheme =
  failwith "Implement applying a substitution to a scheme here"
;;
let apply_subst_env (subst : 'a typ subst) (env : 'a typ envt) : 'a typ envt =
  failwith "Implement applying a substitution to a type environment here"
;;
let apply_subst_funenv (subst : 'a typ subst) (env : 'a scheme envt) : 'a scheme envt =
  failwith "Implement applying a substitution to a scheme environment here"
;;
let apply_subst_subst (subst : 'a typ subst) (dest : 'a typ subst) : 'a typ subst =
  failwith "Implement applying a substitution to another substitution here"
;;
let compose_subst (sub1 : 'a typ subst) (sub2 : 'a typ subst) : 'a typ subst =
  sub1 @ (apply_subst_subst sub1 sub2)
;;

let rec ftv_type (t : 'a typ) : StringSet.t =
  match t with
  | TyBlank _ -> StringSet.empty
  | TyCon _ -> StringSet.empty
  | TyVar(name, _) -> StringSet.singleton name
  | TyArr(args, ret, _) ->
    List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
                    args
                    (ftv_type ret)
  | TyApp(typ, args, _) ->
    List.fold_right (fun t ftvs -> StringSet.union (ftv_type t) ftvs)
                    args
                    (ftv_type typ)
;;
let ftv_scheme (s : 'a scheme) : StringSet.t =
  match s with
  | SForall(args, typ, _) ->
     StringSet.diff (ftv_type typ) (StringSet.of_list args)
let ftv_env (e : 'a typ envt) : StringSet.t =
  failwith "Compute the free type variables of an environment here"
;;
let occurs (name : string) (t : 'a typ) =
  StringSet.mem name (ftv_type t)
;;
exception OccursCheck of string
let bind (tyvarname : string) (t : 'a typ) : 'a typ subst =
  match t with
  | TyVar(name, _) when tyvarname = name -> [] (* nothing to be done *)
  | _ ->
     if StringSet.mem tyvarname (ftv_type t)
     then raise (OccursCheck (sprintf "Infinite types: %s occurs in %s" tyvarname (string_of_typ t)))
     else [(tyvarname, t)]
;;
let ty_err t1 t2 loc reasons = TypeMismatch(loc, t2, t1, reasons)
let rec unify (t1 : 'a typ) (t2 : 'a typ) (loc : sourcespan) (reasons : reason list) : 'a typ subst =
  failwith "Implement type unification"
;;

let gensym =
  let count = ref 0 in
  let next () =
    count := !count + 1;
    !count
  in fun str -> sprintf "%s_%d" str (next ());;

(* Eliminates all `TyBlank`s in a type, and replaces them with fresh type variables *)
let rec unblank (t : 'a typ) : 'a typ =
  match t with
  | TyBlank tag -> TyVar(gensym "blank", tag)
  | TyCon _ -> t
  | TyVar _ -> t
  | TyArr(args, ret, tag) ->
     let args = List.map unblank args in
     let ret = unblank ret in TyArr(args, ret, tag)
  | TyApp(t, args, tag) ->
     let t = unblank t in
     let args = List.map unblank args in TyApp(t, args, tag)
;;


let rec instantiate (s : 'a scheme) : 'a typ =
  match s with
  | SForall([], ftype, loc) -> ftype
  | SForall((v::vs), ftype, loc) ->
      let v1 =  TyVar(gensym "arg", loc) in
      let (args_type, rtype) = (match ftype with
        | TyArr(args, rtype ,  _) -> (args, rtype)
        | _ -> failwith "Expecting Arrow type in scheme types")
      in
      let new_args = List.map
        (fun (a : 'a typ) ->
            match a with
            | TyVar(arg, _) -> (if (compare arg v) == 0 then v1 else a)
            | _ -> a) args_type
      in
      let new_rtype = match rtype with
          | TyVar(rt, _) -> (if (compare rt v) == 0 then v1 else rtype)
          | _ -> rtype
      in
      let new_ftype = TyArr(new_args, new_rtype, loc) in
      let new_scheme = SForall(vs, new_ftype, loc) in
      (instantiate new_scheme)
;;

let generalize (e : 'a typ envt) (t : 'a typ) : 'a scheme =
  failwith "Implement generalizing a type here"
;;

let rec infer_exp (funenv : sourcespan scheme envt) (env : sourcespan typ envt) (e : sourcespan expr) reasons
        : (sourcespan typ subst * sourcespan typ * sourcespan expr) (* unification, result typ, rebuilt expr *) =
  let infer_app (e : sourcespan expr) : (sourcespan typ subst * sourcespan typ * sourcespan expr) =
       let (funname, args, loc) = (match e with
         (* why do we not need to update the environment here with substitutions *)
         | EPrim1(op, arg1, loc) -> ((name_of_op1 op), [arg1] , loc)
         | EPrim2(op, arg1, arg2, loc) -> (name_of_op2 op, [arg1; arg2], loc)
         | EApp(funname, args, loc) -> (funname, args, loc)
         | _ -> failwith "Expecting a function application here.")
       in
       let scheme = (find_pos funenv funname loc) in
       let ftype = (instantiate scheme) in
       (* this is the type that we return *)
       let rtype = TyVar(gensym "arg", loc) in
       let (args_subs, args_types) = List.fold_left
        (fun ((subst_so_far : sourcespan type subst),
               (types_so_far : sourcespan typ list))
               (arg : sourcespan expr) ->
               let (arg_subst, arg_type, _) = (infer_exp funenv env arg reasons) in
               (* order matters in compose *)
               let final_subts = (compose_subst arg_subst subst_so_far) in
               let final_types = types_so_far @ [arg_type] in
               (final_subts, final_types)
          )
          ([], [])
          args
        in
        (* Create a new Arrow type *)
        let arrType = TyArr(args_types, rtype, loc) in
        (* apply these substitions on the arrow type you just created *)
        let arrType = (apply_subst_type args_subs arrType) in
        let final_subts = (unify arrType ftype loc reasons) in
        let final_subts = (compose_subts final_subts args_subs) in
        let final_type = (apply_subst_type final_subs rtype) in
        (final_subts, final_type, e)
  in
  match e with
  | ENumber (n, loc) -> ([], tInt, e)
  | EBool (b, loc) -> ([], tBool, e)
  | EId (x, loc) -> ([], (find_pos env x loc), e)
  | EPrim1(op1, e1, loc) -> (infer_app e)
  | EPrim2(op2, e1, e2, loc) -> (infer_app e)
  | EApp(name, args, loc) -> (infer_app e)
  | ELet(bindings, body, loc) ->
      let (binding_substs, new_env) = List.fold_left
      (fun ((subts_so_far : 'a subst), (env : 'a typ envt))
            (((b, given_btype, _), exp, loc) as b : 'a bind) ->
             let (substs_1, infered_btype, _) = (infer_exp funenv env exp reasons) in
             let substs_2 = (unify infered_btype (unblank given_btype) loc reasons) in
             (* Check the order for compose subst *)
             let final_substs =  (compose_subst substs_2 (compose_subst substs_1 subs_so_far)) in
             let final_type = (apply_subst_typ final_substs infered_btype) in
             (final_substs, StringMap.add x final_type env)
      )
      ([], env)
      bindings
      in
      let (body_substs, rtype, _) = (infer_exp funenv new_env body reasons) in
      (* Composition order is different here *)
      let final_substs = (compose_subst body_substs final_substs) in
      let final_typ = (apply_subst_typ final_substs rtype) in
      (final_substs, final_typ, e)
  | EIf(c, t, f, loc) ->
    let (c_subst, c_typ, c) = infer_exp funenv env c reasons in
    (* Why do we need to apply substitutiont to env in this case. *)
    let env = apply_subst_env c_subst env in
    let (t_subst, t_typ, t) = infer_exp funenv env t reasons in
    let env = apply_subst_env t_subst env in
    let (f_subst, f_typ, f) = infer_exp funenv env f reasons in
    (* Compose the substitutions together *)
    let subst_so_far = compose_subst (compose_subst c_subst t_subst) f_subst in
    (* rewrite the types *)
    let c_typ = apply_subst_typ subst_so_far c_typ in
    let t_typ = apply_subst_typ subst_so_far t_typ in
    let f_typ = apply_subst_typ subst_so_far f_typ in
    (* unify condition with Bool *)
    let unif_subst1 = unify c_typ tBool loc reasons in
    (* unify two branches *)
    let unif_subst2 = unify t_typ f_typ loc reasons in
    (* compose all substitutions *)
    let final_subst = compose_subst (compose_subst subst_so_far unif_subst1) unif_subst2 in
    let final_typ = apply_subst_typ final_subst t_typ in
    (final_subst, final_typ, e)
;;

let infer_decl funenv env (decl : sourcespan decl) reasons : sourcespan scheme envt * sourcespan typ * sourcespan decl =
  match decl with
  | DFun(name, args, scheme, body, loc) ->
    failwith "infer_decl yet to be implemented!!!"

;;
let infer_group funenv env (g : sourcespan decl list) : (sourcespan scheme envt * sourcespan decl list) =
     failwith "Infer group not implemented yet!!"
;;

let infer_prog funenv env (p : sourcespan program) : (sourcespan typ * sourcespan program) =
  match p with
  | Program(declgroups, body, given_type, loc) ->
    let new_funenv =
    List.fold_left (fun fenv g -> let (genv, _) = (infer_group fenv env g) in genv)
    funenv
    declgroups
    in
    (* Infer the body expression for the program *)
    (* Return type of the inferred body expression *)
    let (subst_1, return_type, _) = (infer_exp new_funenv env body []) in
    (* unblank the given type and then unify it with the returned type *)
    let subst_2 = (unify return_type (unblank given_type) loc []) in
    (* Compose all substitutions together  --- order matters *)
    let final_subst = (compose_subst subst_2 subst_1) in
    (* applying the substitions on the return type to get the final type *)
    let final_type = (apply_subst_typ final_subst return_type) in
    (final_type, p)
;;

let type_synth (p : sourcespan program) : sourcespan program fallible =
  try
    (*ptype is used for testing purpose *)
    let (_, p) = infer_prog initial_env StringMap.empty p in Ok(p)
  with e -> Error([e])
;;
