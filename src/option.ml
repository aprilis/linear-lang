let get (Some x) = x
let value default = function
  | Some x -> x
  | None -> default
let bind f = function
  | Some x -> f x
  | None -> None
let map f = bind (fun x -> Some (f x))