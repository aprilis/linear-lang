open Parsing
open Lexing

let parse lexbuf =
  try Parser.prog Lexer.read lexbuf
  with Parser.Error -> failwith "Syntax error"

let parse_file fname =
  let inx = open_in fname in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  let (td, e) = parse lexbuf in
  close_in inx;
  Pretty.printEndline Format.std_formatter Pretty.printExpr e

let main () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in
  List.map parse_file args;
  ()

let () = main ()