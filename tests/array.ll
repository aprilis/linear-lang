let fold = fix (fun <?a, ?b> (rec : (!a -> !b -o !b) -> [!a] -o !b -o !b) ->
                fun (f : (!a -> !b -o !b)) ->
                fun (l : [!a]) -o fun (x : !b) -o
                    case l of
                        | [] -> x
                        | h::t -> f h (rec f t x))
    in
let range = fix (fun (f : int -> [int]) ->
                 fun (x : int) ->
    if x == 0 then [] else x - 1 :: f (x - 1))
    in
let n = 10 in
let rng = range n in
let arr1 = arr_from_elem n 1 in
let arr2 = arr_from_list rng in
let {arr1, arr2} arr = 
    fold (
        fun (i : int) -> 
        fun (arr : ![|int|]) -o
            update arr i (lookup arr1 i + lookup arr2 i))
        rng (arr_from_elem n 0)
    in let {arr} n = fold (
        fun (i : int) ->
        fun (n : int) -o
            n * lookup arr i)
        rng 1 in
    drop arr1; drop arr2; drop arr; n