open Types
open Util
module StrEnv = Map.Make(String)

exception RepeatedVariable of string * string pattern
exception UnboundVariable of string
exception TypeError of string * int expr

type op_env = Types.operator -> (string Types.typ * string Types.typ * string Types.typ)
type typ_env = unit

let fresh_int_id =
  let c = ref 0
  in fun () ->
     let x = !c
     in c := x + 1; x

let rec fold_map f a l =
  match l with
    | [] -> (a, [])
    | h::t -> 
        let (a, t) = fold_map f a t
        in let (a, h) = f a h
        in (a, h::t)

let union = StrEnv.union (fun _ _ x -> Some x)

let pattern_to_int_ids p =
  let update_map x y =
    let f y0 =
      match y0 with
        | None -> Some y
        | Some _ -> raise (RepeatedVariable (x, p))
    in StrEnv.update x f
  in let rec aux env p =
    match p with
      | PVar x ->
          let id = fresh_int_id ()
          in (update_map x id env, PVar id)
      | PTuple pts ->
          let (env, pts) = fold_map aux env pts
          in (env, PTuple pts)
      | PCons (h, t) ->
          let (env, h) = aux env h
          in let (env, t) = aux env t
          in (env, PCons (h, t))
      | PEmptyList -> (env, PEmptyList)
      | PWild -> (env, PWild)
      | PConstr (c, None) -> (env, PConstr (c, None))
      | PConstr (c, Some pp) ->
          let (env, pp) = aux env pp
          in (env, PConstr (c, Some pp))
  in aux StrEnv.empty p

let var_to_int_id env x =
  match StrEnv.find_opt x env with
    | Some y -> y
    | None -> raise (UnboundVariable x)

let to_int_ids =
  let rec aux env e =
    match e with
      | EFun (lin, p, t, e) ->
          let (env1, p) = pattern_to_int_ids p
          in EFun (lin, p, t, aux (union env env1) e)
      | EROLet (ro, p, e, e1) ->
          let (env1, p) = pattern_to_int_ids p
          in EROLet(List.map (var_to_int_id env) ro, p, aux env e, aux (union env env1) e1)
      | ELet (p, e, e1) ->
          let (env1, p) = pattern_to_int_ids p
          in ELet (p, aux env e, aux (union env env1) e1)
      | ECase (e, m) ->
          ECase (aux env e, m |> List.map 
            (fun (p, e) -> let (env1, p) = pattern_to_int_ids p in (p, aux (union env env1) e)))
      | EIf (e1, e2, e3) ->
          EIf (aux env e1, aux env e2, aux env e3)
      | EOp (op, a, b) -> EOp (op, aux env a, aux env b)
      | EApp (a, b) -> EApp (aux env a, aux env b)
      | ETuple l -> ETuple (List.map (aux env) l)
      | EEmptyList -> EEmptyList
      | EArray l -> EArray (List.map (aux env) l)
      | EInt x -> EInt x
      | EString x -> EString x
      | EVar x -> EVar (var_to_int_id env x)
  in aux StrEnv.empty


let non_lin_env = IntEnv.filter (fun _ (lin, _) -> not lin)
let lin_env = IntEnv.filter (fun _ (lin, _) -> lin)

let (--) a b = IntEnv.filter (fun k _ -> not (IntEnv.mem k b)) a
let (++) = IntEnv.union (fun _ _ x -> Some x)

let rec check_pattern p t =
  IntEnv.empty

let failwith msg e = raise (TypeError (msg, e))

let rec nonlinear (lin, t) =
  if lin then
    (false, match t with
      | TPrim _
      | TNonLinVar _
      | TVar _ -> t
      | TFunc (_, _) -> TPrim "void"
      | TTuple l -> TTuple (List.map nonlinear l)
      | TList t -> TList (nonlinear t)
      | TArray t -> TArray (nonlinear t)
    )
  else (lin, t)

let assert_all_used e not_used =
  if not @@ IntEnv.is_empty not_used then
    let not_used_var = fst @@ IntEnv.choose not_used in
    failwith (Format.sprintf "Unused linear value `%d`" not_used_var) e

let assert_safe e ro t = ()

let disjoint_union e =
  IntEnv.union (fun x _ _ -> failwith (Format.sprintf "Linear variable `%d` used twice" x) e)

let swap_pair (a, b) = (b, a)

let print_type ppf t =
  let f (a, b) = Format.sprintf "%s%d" b a in
  Pretty.print_type ppf (map_var f t)

let unify_all e (h::t) =
  try List.fold_left Unify.unify h t with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a %a: %s" print_type t1 print_type t2 msg) e

let unify_and_infer e a b c =
  try Unify.unify_and_infer a b c with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a %a: %s" print_type t1 print_type t2 msg) e

let same_used e (h::t) =
  let eq a b = IntEnv.bindings a = IntEnv.bindings b in
  if List.for_all (eq h) t then h
  else failwith "Inconsistent linear variables usage" e

let auto_lin t = (linear t, t)

let infer_type types lin_types operators =
  let rec aux env ee =
    match ee with
      | EFun (lin, p, t, e) ->
          let env_p = check_pattern p t in
          let env1 = if lin then env else non_lin_env env in
          let (t1, used) = aux (env1 ++ env_p) e in
          let not_used = lin_env env_p -- used in
          assert_all_used ee not_used;
          ((lin, TFunc (t, t1)), used -- env_p)
      | EROLet (ro, p, e, e1) ->
          let lin = lin_env env in
            List.iter (fun x ->
              if not @@ IntEnv.mem x lin then
                failwith (Format.sprintf "Variable `%d` not found or non-linear" x) ee)
              ro;
          let env_ro = List.fold_right (fun x -> IntEnv.add x (nonlinear @@ IntEnv.find x lin))
            ro IntEnv.empty in
          let (t, used) = aux (env ++ env_ro) e in
          let env_p = check_pattern p t in
          let (t1, used1) = aux (env ++ env_p) e1 in
          let not_used = (env_ro ++ lin_env env_p) -- used1 in
          assert_all_used ee not_used;
          assert_safe ee ro t;
          (t1, disjoint_union ee used (used1 -- env_p))
      | ELet (p, e, e1) ->
          let (t, used) = aux env e in
          let env_p = check_pattern p t in
          let (t1, used1) = aux (env ++ env_p) e1 in
          let not_used = lin_env env_p -- used1 in
          assert_all_used ee not_used;
          (t1, disjoint_union ee used (used1 -- env_p))
      | ECase (e, l) ->
          let (t, used) = aux env e in
          let match_item (p, e1) =
            let env_p = check_pattern p t in
            let (t1, used1) = aux (env ++ env_p) e1 in
            let not_used = lin_env env_p -- used1 in
            assert_all_used ee not_used;
            (t1, used1 -- env_p)
            in
          let (t1, used1) = l |> List.map match_item |> List.split in
          (unify_all ee t1, disjoint_union ee used (same_used ee used1))
      | EIf (e, e1, e2) ->
          let (t, used) = aux env e in
          let (t1, used1) = [e1; e2] |> List.map (aux env) |> List.split in
          ignore (unify_all ee [t; (false, TPrim "bool")]);
          (unify_all ee t1, disjoint_union ee used (same_used ee used1))
      | EOp (op, a, b) ->
          let (ta, tb, tr) = operators op in
          let it, used = aux_list env ee [a; b] in
          (unify_and_infer ee [ta; tb] it tr, used)
      | EApp (e1, e2) ->
          let ([t1; t2], used) = aux_list env ee [e1; e2] in
          (match t1 with
            | (_, TFunc (a, b)) ->
                (unify_and_infer ee [a] [t2] b, used)
            | _ -> failwith (Format.asprintf "Expected function, got %a" Pretty.print_type t1) ee)
      | ETuple l ->
          let (t, used) = aux_list env ee l in
          (auto_lin @@ TTuple t, used)
      | EArray l ->
          let (t, used) = aux_list env ee l in
          (auto_lin @@ TArray (unify_all ee t), used)
      | EEmptyList -> ((false, TList (false, TVar "a")), IntEnv.empty)
      | EString x -> ((false, TPrim "string"), IntEnv.empty)
      | EInt x -> ((false, TPrim "int"), IntEnv.empty)
      | EVar x -> 
          let (l, t) = IntEnv.find x env in
          ((l, t), if l then IntEnv.singleton x (l, t) else IntEnv.empty)

  and aux_list env ee el =
    el 
    |> fold_map (fun used e ->
      let (t, used1) = aux env e in
      (disjoint_union ee used used1, t)) IntEnv.empty
    |> swap_pair
    
  in fst << aux IntEnv.empty