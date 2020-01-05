open Types
open Util

type var_t = int * string 
exception Error of string * var_t Types.typ * var_t Types.typ

module VarSet = Set.Make(struct type t = var_t let compare = compare end)

let fresh_names () =
  let names = seq_to_stream names in
  let mapping = Hashtbl.create 16 in
  fun x ->
    (if not @@ Hashtbl.mem mapping x then
      Hashtbl.add mapping x (Stream.next names));
    Hashtbl.find mapping x

let unify_var_t a b =
  let mapping = Hashtbl.create 16 in
  let rec aux used t1 t2 =
    let failwith msg = raise @@ Error (msg, t1, t2) in
    let both a b = if a = b then a else failwith "Linearity mismatch" in
    if t1 = t2 then t1 else
    match (t1, t2) with
      | TFunc (l1, a1, b1), TFunc (l2, a2, b2) ->
          TFunc (both l1 l2, aux used a1 b1, aux used a2 b2)
      | TTuple tl1, TTuple tl2 when List.length tl1 = List.length tl2 ->
          TTuple (List.map2 (aux used) tl1 tl2)
      | TList tl1, TList tl2 ->
          TList (aux used tl1 tl2)
      | TArray tl1, TArray tl2 ->
          TArray (aux used tl1 tl2)
      | TVar (_, x), _ when VarSet.mem x used ->
          failwith "Circular type dependency"
      | _, TVar (_, x) when VarSet.mem x used ->
          failwith "Circular type dependency"
      | TVar (l, x), t2
      | t2, TVar (l, x) ->
        begin
          match Hashtbl.find_opt mapping x with
            | Some t -> aux (VarSet.add x used) t t2
            | None ->
              if is_linear t2 => l then
                (Hashtbl.add mapping x t2; t2)
              else failwith "Linearity mismatch"
        end
      | _ -> failwith "Type mismatch"
  and expand_variables used =
    map (function
        | TVar (_, x) when Hashtbl.mem mapping x ->
            if VarSet.mem x used then
              raise @@ Error ("Circular type dependency", a, b)
            else expand_variables (VarSet.add x used) (Hashtbl.find mapping x)
        | t -> t)
    in
  let u = aux VarSet.empty a b in
  let u = expand_variables VarSet.empty u in
  map_var (fresh_names ()) u 

let unify a b =
  let a = map_var (fun x -> (1, x)) a and b = map_var (fun x -> (2, x)) b in
  unify_var_t a b

let unify_and_infer a b c =
  let a = List.map (map_var (fun x -> (0, x))) a
  and b = List.mapi (fun i -> map_var (fun x -> (i + 1, x))) b
  and c = map_var (fun x -> (0, x)) c in
  let (TTuple l) = unify_var_t (TTuple (c::a)) (TTuple (c::b))
  in List.hd l