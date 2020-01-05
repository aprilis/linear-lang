type 'a str_env = 'a Map.Make(String).t

let (=>) a b = not a || b

let (<<) a b x = a (b x)
let (>>) a b x = b (a x)

let rec filter_map f = function
  | [] -> []
  | h::t ->
    let t = filter_map f t in
    match f h with None -> t | Some h -> h::t


let rec fold_map f a l =
  match l with
    | [] -> (a, [])
    | h::t -> 
        let (a, t) = fold_map f a t
        in let (a, h) = f a h
        in (a, h::t)

let string_of_chars chars = 
  let buf = Buffer.create 1 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let seq_to_stream s =
  let r = ref s in
  Stream.from (fun _ ->
    match !r () with
      | Seq.Nil -> None
      | Seq.Cons (h, t) -> r := t; Some h
  )

let alphabet = String.to_seq "abcdefghijklmnopqrstuvwxyz"
let rec names_list () =
    Seq.Cons([], Seq.flat_map (fun x -> Seq.map (fun y -> y::x) alphabet) names_list)
let names =
  let Seq.Cons(_, t) = names_list () in
  t |> Seq.map List.rev |> Seq.map string_of_chars

module StrEnv = Map.Make(String)
let (++) a b = StrEnv.union (fun _ _ x -> Some x) a b
let (--) a b = StrEnv.filter (fun k _ -> not (StrEnv.mem k b)) a

let const x _ = x
let negate f = not << f
let id x = x