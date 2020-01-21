let map = fun <?a> (f : !a -> !a) ->
  fix (
    fun (rec : int -> ![|!a|] -> ![|!a|]) ->
      fun (i : int) ->
      fun (arr : ![|!a|]) ->
        let {arr} n = len arr in
        if i == n then arr else
          update f i arr |> rec (i + 1)
  ) 0
in map (fun (x : int) -> x * x) [| 1, 2, 3, 4 |]