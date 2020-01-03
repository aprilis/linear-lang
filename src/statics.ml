open Types
open Util
module StrEnv = Map.Make(String)
module IntEnv = Map.Make(struct type t = int let compare = compare end)

exception RepeatedVariable of string * string pattern
exception UnboundVariable of string
exception TypeError of string * int expr

let freshIntId =
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

let patternToIntIds p =
  let updateMap x y =
    let f y0 =
      match y0 with
        | None -> Some y
        | Some _ -> raise (RepeatedVariable (x, p))
    in StrEnv.update x f
  in let rec aux env p =
    match p with
      | PVar x ->
          let id = freshIntId ()
          in (updateMap x id env, PVar id)
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

let varToIntId env x =
  match StrEnv.find_opt x env with
    | Some y -> y
    | None -> raise (UnboundVariable x)

let toIntIds =
  let rec aux env e =
    match e with
      | EFun (lin, p, t, e) ->
          let (env1, p) = patternToIntIds p
          in EFun (lin, p, t, aux (union env env1) e)
      | EROLet (ro, p, e, e1) ->
          let (env1, p) = patternToIntIds p
          in EROLet(List.map (varToIntId env) ro, p, aux env e, aux (union env env1) e1)
      | ELet (p, e, e1) ->
          let (env1, p) = patternToIntIds p
          in ELet (p, aux env e, aux (union env env1) e1)
      | ECase (e, m) ->
          ECase (aux env e, m |> List.map 
            (fun (p, e) -> let (env1, p) = patternToIntIds p in (p, aux (union env env1) e)))
      | EIf (e1, e2, e3) ->
          EIf (aux env e1, aux env e2, aux env e3)
      | EOp (op, a, b) -> EOp (op, aux env a, aux env b)
      | EApp (a, b) -> EApp (aux env a, aux env b)
      | ETuple l -> ETuple (List.map (aux env) l)
      | EEmptyList -> EEmptyList
      | EArray l -> EArray (List.map (aux env) l)
      | EInt x -> EInt x
      | EString x -> EString x
      | EVar x -> EVar (varToIntId env x)
  in aux StrEnv.empty



type type_env = string typ IntEnv.t

let nonLinEnv = IntEnv.filter (fun _ (lin, _) -> not lin)
let linEnv = IntEnv.filter (fun _ (lin, _) -> lin)

let (--) a b = IntEnv.filter (fun k _ -> not (IntEnv.mem k b)) a
let (++) = IntEnv.union (fun _ _ x -> Some x)

let rec checkPattern p t =
  IntEnv.empty

let failwith msg e = raise (TypeError (msg, e))

let rec nonlinear (lin, t) =
  if lin then
    (false, match t with
      | TPrim _
      | TVar _ -> t
      | TFunc (_, _) -> TPrim "void"
      | TTuple l -> TTuple (List.map nonlinear l)
      | TList t -> TList (nonlinear t)
      | TArray t -> TArray (nonlinear t)
    )
  else (lin, t)

let assertAllUsed e notUsed =
  if not @@ IntEnv.is_empty notUsed then
    let notUsedVar = fst @@ IntEnv.choose notUsed in
    failwith (Format.sprintf "Unused linear value `%d`" notUsedVar) e

let disjointUnion e =
  IntEnv.union (fun x _ _ -> failwith (Format.sprintf "Linear variable `%d` used twice" x) e)

let swapPair (a, b) = (b, a)

let printType ppf t =
  let f (a, b) = Format.sprintf "(%d)%s" a b in
  Pretty.printType ppf (mapVar f t)

let unifyAll e (h::t) =
  try List.fold_left Unify.unify h t with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a %a: %s" printType t1 printType t2 msg) e

let unifyAndInfer e a b c =
  try Unify.unifyAndInfer a b c with
    Unify.Error (msg, t1, t2) ->
      failwith (Format.asprintf "Cannot unify types %a %a: %s" printType t1 printType t2 msg) e

let sameUsed e (h::t) =
  let eq a b = IntEnv.bindings a = IntEnv.bindings b in
  if List.for_all (eq h) t then h
  else failwith "Inconsistent linear variables usage" e

let fixLin t = t
let autoLin t = fixLin (false, t)

let inferType types linTypes operators =
  let rec aux env ee =
    match ee with
      | EFun (lin, p, t, e) ->
          let envP = checkPattern p t in
          let env1 = if lin then env else nonLinEnv env in
          let (t1, used) = aux (env1 ++ envP) e in
          let notUsed = linEnv envP -- used in
          assertAllUsed ee notUsed;
          ((lin, TFunc (t, t1)), used -- envP)
      | EROLet (ro, p, e, e1) ->
          let lin = linEnv env in
            List.iter (fun x ->
              if not @@ IntEnv.mem x lin then
                failwith (Format.sprintf "Variable `%d` not found or non-linear" x) ee)
              ro;
          let envRO = List.fold_right (fun x -> IntEnv.add x (nonlinear @@ IntEnv.find x lin))
            ro IntEnv.empty in
          let (t, used) = aux (env ++ envRO) e in
          let envP = checkPattern p t in
          let (t1, used1) = aux (env ++ envP) e1 in
          let notUsed = (envRO ++ linEnv envP) -- used1 in
          assertAllUsed ee notUsed;
          (t1, disjointUnion ee used used1 -- envP)
      | ELet (p, e, e1) ->
          let (t, used) = aux env e in
          let envP = checkPattern p t in
          let (t1, used1) = aux (env ++ envP) e1 in
          let notUsed = linEnv envP -- used1 in
          assertAllUsed ee notUsed;
          (t1, disjointUnion ee used used1)
      | ECase (e, l) ->
          let (t, used) = aux env e in
          let match_item (p, e1) =
            let envP = checkPattern p t in
            let (t1, used1) = aux (env ++ envP) e1 in
            let notUsed = linEnv envP -- used1 in
            assertAllUsed ee notUsed;
            (t1, used1 -- envP)
            in
          let (t1, used1) = l |> List.map match_item |> List.split in
          (unifyAll ee t1, disjointUnion ee used (sameUsed ee used1))
      | EIf (e, e1, e2) ->
          let [(t, used); (t1, used1); (t2, used2)] = List.map (aux env) [e; e1; e2] in
          (unifyAll ee [t1; t2], disjointUnion ee used (sameUsed ee [used1; used2]))
      | EOp (op, a, b) ->
          let (ta, tb, tr) = operators op in
          let it, used = aux_list env ee [a; b] in
          (unifyAndInfer ee [ta; tb] it tr, used)
      | EApp (e1, e2) ->
          let ([t1; t2], used) = aux_list env ee [e1; e2] in
          (match t1 with
            | (_, TFunc (a, b)) ->
                (unifyAndInfer ee [a] [t2] b, used)
            | _ -> failwith (Format.asprintf "Expected function, got %a" Pretty.printType t1) ee)
      | ETuple l ->
          let (t, used) = aux_list env ee l in
          (autoLin @@ TTuple t, used)
      | EArray l ->
          let (t, used) = aux_list env ee l in
          (autoLin @@ TArray (unifyAll ee t), used)
      | _ -> ((false, TTuple []), IntEnv.empty)
  and aux_list env ee el =
    el 
    |> fold_map (fun used e ->
      let (t, used1) = aux env e in
      (disjointUnion ee used used1, t)) IntEnv.empty
    |> swapPair
  in aux IntEnv.empty