let symbol = Parser.prog_eof

let go prog =
  try
    let (td, e) = prog in
    let env = List.fold_right Type_def.add_to_statics td Prelude.statics_env in
    let t = Statics.infer_type env e in
    print_string "type: ";
    Pretty.print_type Format.std_formatter t;
    let v = Eval.eval Prelude.runtime_env e in
    Pretty.print_value Format.std_formatter v
    
  with 
    | Statics.TypeError (msg, e) ->
      Format.fprintf Format.std_formatter "Type error: %s\nin expression %a@." msg Pretty.print_expr e
    | Eval.RuntimeError msg ->
      Format.printf "Runtime error: %s@." msg

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