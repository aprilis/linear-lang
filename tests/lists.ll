let double = fix (fun (f : forall a . [a] -> [a]) ->
    fun (l : [a]) ->
        case l of
            | [] -> []
            | h::t -> h::h::(f t))
    in
let map = fun (g : forall ?a, ?b . a -> b) ->
    fix (fun (f : [!a] -> [!b]) ->
        fun (l : [!a]) ->
            case l of
                | [] -> []
                | h::t -> (g h)::(f t))
    in
    map (fun (x : int) -> x * x) (double [1, 2, 3, 4])