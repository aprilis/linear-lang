let (<<) a b x = a (b x)
let (>>) a b x = b (a x)

let rec filter_map f = function
  | [] -> []
  | h::t ->
    let t = filter_map f t in
    match f h with None -> t | Some h -> h::t

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

let invert_mapping tbl =
  tbl |> Hashtbl.to_seq |> Seq.map (fun (a, b) -> (b, a)) |> Hashtbl.of_seq