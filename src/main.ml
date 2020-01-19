let symbol = Parser.prog_eof

let go prog =
  try
    let (td, use, e) = prog in
    let env = List.fold_right Type_def.add_to_statics td (Prelude.statics_env use) in
    let t = Statics.infer_type env e in
    let run_env = List.fold_right Type_def.add_to_runtime td (Prelude.runtime_env use) in
    let v = Eval.eval run_env e in
    print_string "\ntype: ";
    Pretty.print_type Format.std_formatter t;
    print_string "value: ";
    Pretty.print_value Format.std_formatter v
    
  with 
    | Statics.TypeError (msg, e) ->
      Format.fprintf Format.std_formatter "Type error: %s\nin expression %a@." msg Pretty.print_expr e
    | Eval.RuntimeError msg ->
      Format.printf "Runtime error: %s@." msg

let read () =
  let buffer = Buffer.create 128 in
  let rec aux () =
    let text = read_line () in
    if text = "" then () else begin
      Buffer.add_string buffer text;
      Buffer.add_char buffer '\n';
      aux ()
    end in
  aux (); Buffer.contents buffer

let rec repl () =
  begin
    let text = read () in
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