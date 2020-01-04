open Types
open Util
module StrEnv = Map.Make(String)

exception RepeatedVariable of string * pattern
exception UnboundVariable of string
exception TypeError of string * expr

type op_env = Types.operator -> (string Types.typ * string Types.typ * string Types.typ)
type env = {
  ops: op_env;
  vars: string Types.typ StrEnv.t;
  types: Types.prim_type StrEnv.t;
  lintypes: Types.prim_type StrEnv.t;
}

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

let non_lin_env = StrEnv.filter (fun _ (lin, _) -> not lin)
let lin_env = StrEnv.filter (fun _ (lin, _) -> lin)

let (--) a b = StrEnv.filter (fun k _ -> not (StrEnv.mem k b)) a
let (++) = StrEnv.union (fun _ _ x -> Some x)

let disjoint_union err =
  StrEnv.union (fun x _ _ -> err x)

let disjoint_union_lin e =
  disjoint_union (fun x -> failwith (Format.sprintf "Linear variable %s used twice" x) e)

let check_pattern e env =
  let failwith msg = raise @@ TypeError (msg, e) in
  let rec aux p t =
    let disjoint_union = disjoint_union (fun x -> raise @@ RepeatedVariable (x, p)) in
    let fail = fun () -> failwith (Format.asprintf "Failed to match pattern %a with type %a"
      Pretty.print_pattern p Pretty.print_type t) in
    match p, t with
      | PVar x, _ -> StrEnv.singleton x t
      | PWild, (false, _) -> StrEnv.empty
      | PTuple pl, (_, TTuple tl) ->
          List.map2 aux pl tl |> List.fold_left (disjoint_union) StrEnv.empty
      | PCons (h, tail), (_, TList tl) ->
          disjoint_union (aux h tl) (aux tail t)
      | PEmptyList, (_, TList _) -> StrEnv.empty
      | PConstr (cl, c, cp), (l, TPrim s) when cl = l ->
          let tenv = if l then env.types else env.lintypes in
          begin match StrEnv.find_opt s tenv with
            | Some pt ->
                begin match (cp, List.assoc_opt c pt) with
                  | Some p, Some (Some t) -> aux p t
                  | None, Some (None) -> StrEnv.empty
                  | _ -> fail ()
                end
            | None -> failwith (Format.sprintf "Failed to expand type %s" s)
          end
      | _ -> fail ()
  in aux

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
  if not @@ StrEnv.is_empty not_used then
    let not_used_var = fst @@ StrEnv.choose not_used in
    failwith (Format.sprintf "Unused linear value %s" not_used_var) e

let assert_safe e ro t = ()

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
  let eq a b = StrEnv.bindings a = StrEnv.bindings b in
  if List.for_all (eq h) t then h
  else failwith "Inconsistent linear variables usage" e

let fresh_types tenv =
  let mapping = Hashtbl.create 1 in
  let names = names |> Seq.filter (fun x -> not @@ StrEnv.mem x tenv) |> seq_to_stream in
  fun x ->
    (if not @@ Hashtbl.mem mapping x then
      Hashtbl.add mapping x (Stream.next names));
    Hashtbl.find mapping x

let arg_to_body_type = map (function
  | (_, TVar x) -> (true, TPrim x)
  | (l, TNonLinVar x) -> (l, TPrim x)
  | x -> x)

let body_to_res_type arg =
  let vars = fold List.cons arg [] |> filter_map (function 
    | _, TVar x -> Some (x, TVar x)
    | _, TNonLinVar x -> Some (x, TNonLinVar x)
    | _ -> None) |> List.to_seq |> StrEnv.of_seq
    in
  map (function
    | (l, TPrim x) when StrEnv.mem x vars -> 
        let y = StrEnv.find x vars in (l && (y = TNonLinVar x), y)
    | x -> x)

let auto_lin t = (linear t, t)

let infer_type env =
  let rec aux venv ee =
    match ee with
      | EFun (lin, p, t, e) ->
          let venv_p = check_pattern ee env p (arg_to_body_type t) in
          let venv1 = if lin then venv else non_lin_env venv in
          let (t1, used) = aux (venv1 ++ venv_p) e in
          let t1 = body_to_res_type t t1 in
          let not_used = lin_env venv_p -- used in
          assert_all_used ee not_used;
          ((lin, TFunc (t, t1)), used -- venv_p)
      | EROLet (ro, p, e, e1) ->
          let lin = lin_env venv in
            List.iter (fun x ->
              if not @@ StrEnv.mem x lin then
                failwith (Format.sprintf "Variable %s not found or non-linear" x) ee)
              ro;
          let venv_ro = List.fold_right (fun x -> StrEnv.add x (nonlinear @@ StrEnv.find x lin))
            ro StrEnv.empty in
          let (t, used) = aux (venv ++ venv_ro) e in
          let venv_p = check_pattern ee env p t in
          let (t1, used1) = aux (venv ++ venv_p) e1 in
          let not_used = (venv_ro ++ lin_env venv_p) -- used1 in
          assert_all_used ee not_used;
          assert_safe ee ro t;
          (t1, disjoint_union_lin ee used (used1 -- venv_p))
      | ELet (p, e, e1) ->
          let (t, used) = aux venv e in
          let venv_p = check_pattern ee env p t in
          let (t1, used1) = aux (venv ++ venv_p) e1 in
          let not_used = lin_env venv_p -- used1 in
          assert_all_used ee not_used;
          (t1, disjoint_union_lin ee used (used1 -- venv_p))
      | ECase (e, l) ->
          let (t, used) = aux venv e in
          let match_item (p, e1) =
            let venv_p = check_pattern ee env p t in
            let (t1, used1) = aux (venv ++ venv_p) e1 in
            let not_used = lin_env venv_p -- used1 in
            assert_all_used ee not_used;
            (t1, used1 -- venv_p)
            in
          let (t1, used1) = l |> List.map match_item |> List.split in
          (unify_all ee t1, disjoint_union_lin ee used (same_used ee used1))
      | EIf (e, e1, e2) ->
          let (t, used) = aux venv e in
          let (t1, used1) = [e1; e2] |> List.map (aux venv) |> List.split in
          ignore (unify_all ee [t; (false, TPrim "bool")]);
          (unify_all ee t1, disjoint_union_lin ee used (same_used ee used1))
      | EOp (op, a, b) ->
          let (ta, tb, tr) = env.ops op in
          let it, used = aux_list venv ee [a; b] in
          (unify_and_infer ee [ta; tb] it tr, used)
      | EApp (e1, e2) ->
          let ([t1; t2], used) = aux_list venv ee [e1; e2] in
          (match t1 with
            | (_, TFunc (a, b)) ->
                (unify_and_infer ee [a] [t2] b, used)
            | _ -> failwith (Format.asprintf "Expected function, got %a" Pretty.print_type t1) ee)
      | ETuple l ->
          let (t, used) = aux_list venv ee l in
          (auto_lin @@ TTuple t, used)
      | EArray l ->
          let (t, used) = aux_list venv ee l in
          (auto_lin @@ TArray (unify_all ee t), used)
      | EEmptyList -> ((false, TList (false, TVar "a")), StrEnv.empty)
      | EString x -> ((false, TPrim "string"), StrEnv.empty)
      | EInt x -> ((false, TPrim "int"), StrEnv.empty)
      | EVar x -> 
          match StrEnv.find_opt x venv with
            | Some (l, t) -> ((l, t), if l then StrEnv.singleton x (l, t) else StrEnv.empty)
            | None -> raise @@ UnboundVariable x

  and aux_list venv ee el =
    el 
    |> fold_map (fun used e ->
      let (t, used1) = aux venv e in
      (disjoint_union_lin ee used used1, t)) StrEnv.empty
    |> swap_pair
    
  in fst << aux env.vars