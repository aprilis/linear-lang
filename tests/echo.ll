use stdin, stdout in
let loop = fix (fun (rec : (!stdin, !stdout) -> (!stdin, !stdout)) ->
    fun ((stdin, stdout) : (!stdin, !stdout)) ->
      let (line_opt, stdin) = read_line stdin in
      case line_opt of
        | Line line ->
            let stdout = stdout |> print line |> print "\n" in
            rec (stdin, stdout)
        | EOF -> (stdin, stdout))
  in
loop (stdin, stdout)