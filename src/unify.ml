open Types
open Util

type var_t = int * string 
exception Error of string * var_t Types.typ * var_t Types.typ

module VarSet = Set.Make(struct type t = var_t let compare = compare end)

let string_of_chars chars = 
  let buf = Buffer.create 1 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let alphabet = String.to_seq "abcdefghijklmnopqrstuvwxyz"
let rec names_list () =
   Seq.Cons([], Seq.flat_map (fun x -> Seq.map (fun y -> y::x) alphabet) names_list)
let names =
  let Seq.Cons(_, t) = names_list () in
  t |> Seq.map List.rev |> Seq.map string_of_chars

let fresh_names () =
  let names = ref names in
  let mapping = Hashtbl.create 16 in
  fun x ->
    (if not @@ Hashtbl.mem mapping x then
      let Seq.Cons (h, t) = !names () in
      Hashtbl.add mapping x h;
      names := t);
    Hashtbl.find mapping x

let unify_var_t a b =
  let mapping = Hashtbl.create 16 in
  let rec aux used (l1, t1) (l2, t2) =
    let failwith msg = raise @@ Error (msg, (l1, t1), (l2, t2)) in
    if l1 <> l2 then
      failwith "Linearity mismatch"
    else let t = if t1 == t2 then t1 
      else match (t1, t2) with
        | TFunc (a1, b1), TFunc (a2, b2) ->
            TFunc (aux used a1 b1, aux used a2 b2)
        | TTuple tl1, TTuple tl2 when List.length tl1 = List.length tl2 ->
            TTuple (List.map2 (aux used) tl1 tl2)
        | TList tl1, TList tl2 ->
            TList (aux used tl1 tl2)
        | TArray tl1, TArray tl2 ->
            TArray (aux used tl1 tl2)
        | TVar x, _ when VarSet.mem x used ->
            failwith "Circular type dependency"
        | _, TVar x when VarSet.mem x used ->
            failwith "Circular type dependency"
        | TVar x, _ ->
          begin
            match Hashtbl.find_opt mapping x with
              | Some y -> snd @@ aux (VarSet.add x used) (l1, y) (l2, t2)
              | None -> Hashtbl.add mapping x t2; t2
          end
        | _, TVar x ->
          begin
            match Hashtbl.find_opt mapping x with
              | Some y -> snd @@ aux (VarSet.add x used) (l1, t1) (l2, y)
              | None -> Hashtbl.add mapping x t1; t1
          end
        | _ -> failwith "Type mismatch"
    in (l1, t) in
  let u = aux VarSet.empty a b in
  map_var (fresh_names ()) u 

let unify a b =
  let a = map_var (fun x -> (1, x)) a and b = map_var (fun x -> (2, x)) b in
  unify_var_t a b

let unify_and_infer a b c =
  let a = List.map (map_var (fun x -> (1, x))) a
  and b = List.map (map_var (fun x -> (2, x))) b
  and c = map_var (fun x -> (1, x)) c in
  let (_, TTuple l) = unify_var_t (false, TTuple (c::a)) (false, TTuple (c::b))
  in List.hd l