open Parsing
open Lexing

let parse lexbuf =
  try Parser.prog Lexer.read lexbuf
  with Parser.Error -> failwith "Syntax error"

let rec main () =
  begin
    let text = read_line ()
    in let lexbuf = Lexing.from_string text
    in try
        let (td, e) = Parser.prog Lexer.read lexbuf
        in Pretty.print_int_expr Format.std_formatter (Statics.to_int_ids e);
      with Parser.Error -> print_endline "Syntax error"
  end;
  main()

let () = main ()