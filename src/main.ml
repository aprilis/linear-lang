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
        in let t = Statics.infer_type Prelude.statics_env e
        in Pretty.print_expr Format.std_formatter e;
           Pretty.print_type Format.std_formatter t
          
      with Parser.Error -> print_endline "Syntax error"
  end;
  main()

let () = main ()