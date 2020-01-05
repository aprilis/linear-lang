let symbol = Parser.prog_eof

let go prog =
  let (td, e) = prog in
  let env = List.fold_right Type_def.add_type_def td Prelude.statics_env in
  let t = Statics.infer_type env e in
  Pretty.print_expr Format.std_formatter e;
  Pretty.print_type Format.std_formatter t

let rec repl () =
  begin
    let text = read_line () in
    try
      go @@ Parse.parse_text symbol text
    with Parser.Error -> print_endline "Syntax error"
  end;
  repl()

let parse_file fname =
  go @@ Parse.parse_file symbol fname

let () =
  match Array.length Sys.argv with
    | 1 -> repl ()
    | 2 -> parse_file Sys.argv.(1)
    | _ -> failwith "Invalid number of arguments"