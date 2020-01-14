open Types
open Util
module StrEnv = Map.Make(String)

exception RepeatedVariable of string * pattern
exception TypeError of string * expr

exception TypeError_ of string
let failwith msg = raise @@ TypeError_ msg

type op_env = operator -> string typ
type env = {
  ops: op_env;
  vars: string typ str_env;
  types: prim_type str_env;
  lintypes: prim_type str_env;
  valid_types: linearity str_env;
  type_vars: (string * var_linearity) str_env;
}

let non_lin_env = StrEnv.filter (const (negate is_linear))
let lin_env = StrEnv.filter (const is_linear)

let disjoint_union err =
  StrEnv.union (fun x _ _ -> err x)

let disjoint_union_lin =
  disjoint_union (fun x -> failwith (Format.sprintf "Linear variable %s used twice" x))

let check_pattern env =
  let rec aux p t =
    let disjoint_union = disjoint_union (fun x -> raise @@ RepeatedVariable (x, p)) in
    let fail = fun () -> failwith (Format.asprintf "Failed to match pattern %a with type %a"
      Pretty.print_pattern p Pretty.print_type t) in
    match p, t with
      | PVar x, _ -> StrEnv.singleton x t
      | PWild, _ when not (is_linear t) -> StrEnv.empty
      | PTuple pl, TTuple tl ->
          List.map2 aux pl tl |> List.fold_left (disjoint_union) StrEnv.empty
      | PCons (h, tail), TList tl ->
          disjoint_union (aux h tl) (aux tail t)
      | PEmptyList, TList _ -> StrEnv.empty
      | PConstr (cl, c, cp), TPrim (l, s) when cl = l ->
          let tenv = if l then env.lintypes else env.types in
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

let assert_all_used not_used =
  if not @@ StrEnv.is_empty not_used then
    let not_used_var = fst @@ StrEnv.choose not_used in
    failwith (Format.sprintf "Unused linear value %s" not_used_var)

module BoolStrSet = Set.Make(struct type t = bool * string let compare = compare end)

let is_disjoint s1 s2 = 
  not @@ BoolStrSet.exists (fun x -> BoolStrSet.mem x s1) s2

let components env t = 
  let rec aux t s = fold (fun t s ->
    match t with
    | TPrim (l, x) when not @@ BoolStrSet.mem (l, x) s ->
        let tenv = if l then env.lintypes else env.types in
        begin match StrEnv.find_opt x tenv with
          | Some pt -> List.fold_right aux (filter_map snd pt) s
          | None -> s
        end
    | _ -> s    
  ) t s
  in aux t BoolStrSet.empty

let has_function t = fold (fun t ok ->
  match t with TFunc (_, _, _) -> true | _ -> ok) t false

let is_safe env a b =
  let ca = components env a |> BoolStrSet.filter fst |> BoolStrSet.map (fun (_, x) -> (false, x))
  and cb = components env b |> BoolStrSet.filter (negate fst)
  in is_disjoint ca cb && not @@ has_function b

let assert_safe env ro t =
  List.iter (fun x ->
    if not @@ is_safe env x t then failwith (Format.asprintf "Type %a is not safe for %a" 
      Pretty.print_type t Pretty.print_type x)
  ) ro

let swap_pair (a, b) = (b, a)

let print_type ppf t =
  let f (a, b) = Format.sprintf "%s%d" b a in
  Pretty.print_type ppf (map_var f t)

let unify_all l =
  try List.fold_right Unify.unify l (TVar (AnyLin, "a")) with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a and %a: %s" print_type t1 print_type t2 msg)

let unify_and_infer a b c =
  try Unify.unify_and_infer a b c with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a and %a: %s" print_type t1 print_type t2 msg)

let same_used (h::t) =
  let eq a b = StrEnv.bindings a = StrEnv.bindings b in
  if List.for_all (eq h) t then h
  else failwith "Inconsistent linear variables usage"

let body_to_res_type vars =
  let to_vars = vars 
    |> StrEnv.to_seq 
    |> Seq.map (fun (a, (b, l)) -> (b, (a, l))) 
    |> StrEnv.of_seq in
  map (function
    | TPrim (l, x) when StrEnv.mem x to_vars -> 
        let (y, l1) = StrEnv.find x to_vars in
        TVar ((if l then l1 else NonLin), y)
    | x -> x)

let type_vars fresh_types tv =
  let renamed, lin = 
  tv
  |> List.map (fun (x, l) ->
      let fresh = Stream.next fresh_types in
      ((x, (fresh, l)), (fresh, accepts_linear l))
      )
  |> List.split
  in (renamed |> List.to_seq |> StrEnv.of_seq, lin |> List.to_seq |> StrEnv.of_seq)

let assert_valid_types env =
  let is_invalid l1 = function
    | Some l2 when l1 => l2 -> false
    | _ -> true
    in
  map (function
    | TPrim (l, x) when is_invalid l (StrEnv.find_opt x env.valid_types) ->
          failwith (Format.asprintf "Unknown type %a" Pretty.print_type (TPrim (l, x)))
    | t -> t) >> ignore

let translate_types env =
  map (function
    | TPrim (l, x) when StrEnv.mem x env.type_vars ->
        let (y, l1) = StrEnv.find x env.type_vars in
        begin match l, l1 with
          | false, AnyLin
          | true, NonLin -> failwith ("Invalid linearity of type variable "
                                   ^ (if l then "!" else "") ^ x)
          | _ -> TPrim (l, y)
        end
    | t -> t)

let infer_type env =
  let fresh_types = 
    names |> Seq.filter (fun x -> not @@ StrEnv.mem x env.types) |> seq_to_stream
    in
  let rec aux env ee =
    try
      let venv = env.vars in
      let res =
      match ee with
        | EFun (lin, tv, p, t, e) ->
            let vars, vars_lin = type_vars fresh_types tv in
            let env = { env with 
              type_vars = env.type_vars ++ vars;
              valid_types = env.valid_types ++ vars_lin } in
            let t = translate_types env t in
            assert_valid_types env t;
            let venv_p = check_pattern env p t in
            let venv1 = if lin then venv else non_lin_env venv in
            let (t1, used) = aux { env with vars = venv1 ++ venv_p } e in
            let t1 = body_to_res_type vars t1 in
            let t = body_to_res_type vars t in
            let not_used = lin_env venv_p -- used in
            assert_all_used not_used;
            (TFunc (lin, t, t1), used -- venv_p)
        | EROLet (ro, p, e, e1) ->
            let lin = lin_env venv in
              List.iter (fun x ->
                if not @@ StrEnv.mem x lin then
                  failwith (Format.sprintf "Variable %s not found or non-linear" x))
                ro;
            let venv_ro = List.fold_right (fun x -> StrEnv.add x (nonlinear @@ StrEnv.find x lin))
              ro StrEnv.empty in
            let (t, used) = aux { env with vars = venv ++ venv_ro } e in
            let venv_p = check_pattern env p t in
            let (t1, used1) = aux { env with vars = venv ++ venv_p } e1 in
            let not_used = (venv_ro ++ lin_env venv_p) -- used1 in
            assert_all_used not_used;
            assert_safe env (List.map (fun x -> StrEnv.find x lin) ro) t;
            (t1, disjoint_union_lin used (used1 -- venv_p))
        | ELet (p, e, e1) ->
            let (t, used) = aux env e in
            let venv_p = check_pattern env p t in
            let (t1, used1) = aux { env with vars = venv ++ venv_p } e1 in
            let not_used = lin_env venv_p -- used1 in
            assert_all_used not_used;
            (t1, disjoint_union_lin used (used1 -- venv_p))
        | ECase (e, l) ->
            let (t, used) = aux env e in
            let match_item (p, e1) =
              let venv_p = check_pattern env p t in
              let (t1, used1) = aux { env with vars = venv ++ venv_p } e1 in
              let not_used = lin_env venv_p -- used1 in
              assert_all_used not_used;
              (t1, used1 -- venv_p)
              in
            let (t1, used1) = l |> List.map match_item |> List.split in
            (unify_all t1, disjoint_union_lin used (same_used used1))
        | EIf (e, e1, e2) ->
            let (t, used) = aux env e in
            let (t1, used1) = [e1; e2] |> List.map (aux env) |> List.split in
            ignore (unify_all [t; TPrim (false, "bool")]);
            (unify_all t1, disjoint_union_lin used (same_used used1))
        | EOp (op, a, b) ->
            let TFunc (_, TTuple [ta; tb], tr) = env.ops op in
            let it, used = aux_list env [a; b] in
            (unify_and_infer [ta; tb] it tr, used)
        | EApp (e1, e2) ->
            let ([t1; t2], used) = aux_list env [e1; e2] in
            (match t1 with
              | TFunc (_, a, b) ->
                  (unify_and_infer [a] [t2] b, used)
              | _ -> failwith (Format.asprintf "Expected function, got %a" Pretty.print_type t1))
        | ETuple l ->
            let (t, used) = aux_list env l in
            (TTuple t, used)
        | EArray a ->
            let (t, used) = aux_list env a in
            (TArray (true, unify_all t), used)
        | EEmptyList -> (TList (TVar (AnyLin, "a")), StrEnv.empty)
        | EString x -> (TPrim (false, "string"), StrEnv.empty)
        | EInt x -> (TPrim (false, "int"), StrEnv.empty)
        | EVar x -> 
            match StrEnv.find_opt x venv with
              | Some t -> (t, if is_linear t then StrEnv.singleton x t else StrEnv.empty)
              | None -> failwith ("Unbound variable " ^ x)
        in (* Format.fprintf Format.std_formatter "%a : %a\n" Pretty.print_type (fst res) Pretty.print_expr ee; *) 
        res
      with TypeError_ msg -> raise @@ TypeError (msg, ee)

  and aux_list env el =
    el 
    |> fold_map (fun used e ->
      let (t, used1) = aux env e in
      (disjoint_union_lin used used1, t)) StrEnv.empty
    |> swap_pair
    
  in fst << aux env