type !io = IO (!stdin, !stdout)

use stdin, stdout in
let io_print = 
  fun (x : [char]) ->
  fun (!IO (stdin, stdout) : !io) ->
    let {} stdout = print x stdout in
    !IO (stdin, stdout)
  in
let io_read_line =
  fun (!IO (stdin, stdout) : !io) ->
    let {} (line_opt, stdin) = read_line stdin in
    (line_opt, !IO (stdin, stdout))
  in
let loop = fix (
  fun (rec : !io -> !io) ->
  fun (stdio : !io) ->
    let (line_opt, stdio) = io_read_line stdio in
    case line_opt of
      | Line line ->
          let stdio = stdio |> io_print line |> io_print "\n" in
          rec stdio
      | EOF -> stdio)
  in
let stdio = !IO (stdin, stdout) in
loop stdio