use stdin, stdout in
let (line, stdin) = read_line stdin in
print line stdout |> print "\n" |>
drop; drop stdin