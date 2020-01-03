let (<<) a b x = a (b x)

let (=>) a b = not b || a

let const x _ = x

let rec filter_map f = function
  | [] -> []
  | h::t ->
    let t = filter_map f t in
    match f h with None -> t | Some h -> h::t