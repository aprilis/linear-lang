open Types
open Util
module StrEnv = Map.Make(String)

type value = 
  | VInt of int 
  | VString of string 
  | VCons of lazy_value * lazy_value
  | VEmptyList
  | VTuple of lazy_value list
  | VArray of lazy_value array
  | VConstr of string * lazy_value option
  | VFunc of (lazy_value -> lazy_value)
and lazy_value = value Lazy.t

type op_env = operator -> lazy_value -> lazy_value -> value
type env = {
  ops: op_env;
  vars: value StrEnv.t;
}

exception RuntimeError of string

let pm_get = function
  | Some x -> x
  | None -> raise @@ RuntimeError "Pattern matching failed"

let rec find_map f = function
  | h::t -> (match f h with None -> find_map f t | x -> x)
  | [] -> None

exception MatchFailure

let eval prelude =
  let rec bind_pattern_impl v p =
    match p, v with
      | PVar x, _ -> StrEnv.singleton x v
      | PWild, _ -> StrEnv.empty
      | PTuple tp, lazy (VTuple tv) ->
          List.fold_right2 (fun v p env -> bind_pattern_impl v p ++ env) tv tp StrEnv.empty
      | PCons (ph, pt), lazy (VCons (vh, vt)) ->
          bind_pattern_impl vh ph ++ bind_pattern_impl vt pt
      | PEmptyList, lazy (VEmptyList) -> StrEnv.empty
      | PConstr (_, pn, pc), lazy (VConstr (vn, vc)) when pn = vn ->
          begin match pc, vc with
            | None, None -> StrEnv.empty
            | Some pc, Some vc -> bind_pattern_impl vc pc
          end
      | _, _ -> raise MatchFailure
  and bind_pattern_opt env v p =
    try Some (env ++ bind_pattern_impl v p) with
      MatchFailure -> None
  and bind_pattern env v = pm_get << bind_pattern_opt env v
  and eval env = function
    | EFun (_, _, p, _, e) ->
        VFunc (fun x -> lazy_eval (bind_pattern env x p) e)
    | ELet (p, e, e1) ->
        let x = lazy_eval env e in
        eval (bind_pattern env x p) e1
    | EROLet (_, p, e, e1) ->
        let x = lazy_eval env e in
        force_eager x;
        eval (bind_pattern env x p) e1
    | ECase (e, m) ->
        let x = lazy_eval env e in
        find_map (fun (p, e1) -> bind_pattern_opt env x p |> Option.map (flip eval e1)) m
          |> pm_get
    | EIf (cond, a, b) ->
        let x = eval env cond in
        begin match x with
          | VConstr ("True", None) -> eval env a
          | VConstr ("False", None) -> eval env b
        end
    | EOp (op, a, b) ->
        let f = prelude.ops op in
        f (lazy_eval env a) (lazy_eval env b)
    | EApp (a, b) ->
        let VFunc f = eval env a in
        Lazy.force @@ f (lazy_eval env b)
    | ETuple t ->
        VTuple (List.map (lazy_eval env) t)
    | EEmptyList -> VEmptyList
    | EArray l ->
        VArray (l |> List.map (lazy_eval env) |> Array.of_list)
    | EInt x -> VInt x
    | EString x -> VString x
    | EVar x -> Lazy.force @@ StrEnv.find x env
  and lazy_eval env e = lazy (eval env e)
  and force_eager (lazy v) =
    match v with
      | VCons (h, t) -> force_eager h; force_eager t
      | VTuple l -> List.iter force_eager l
      | VArray a -> Array.iter force_eager a
      | VConstr (_, Some c) -> force_eager c
      | _ -> ()
  in lazy_eval (StrEnv.map Lazy.from_val prelude.vars)

let rec to_list (lazy v) = match v with
  | VCons (h, t) -> h :: to_list t
  | VEmptyList -> []