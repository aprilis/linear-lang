let parse symbol lexbuf =
  symbol Lexer.read lexbuf

let parse_text symbol text =
  parse symbol (Lexing.from_string text)

let parse_file symbol fname =
  let inx = open_in fname in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  parse symbol lexbuf