{
open Lexing
open Parser

let escape_chars = [
  ('t', '\t');
  ('n', '\n');
  ('r', '\r');
  ('\\', '\\');
  ('"', '"');
  ('\'', '\'')
]

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int = '-'? ['0'-'9'] ['0'-'9']*
let idchar = ['a'-'z' 'A'-'Z' '_']
let varid = ['a'-'z' '_'] (idchar | ['0'-'9'])*
let constrid = ['A'-'Z'] (idchar | ['0'-'9'])*
let escaped = '\\' ['t' 'n' 'r' '\\' '"' '\'']

rule read =
  parse
  | white {read lexbuf}
  | newline {new_line lexbuf; read lexbuf}
  | int {INT (int_of_string (Lexing.lexeme lexbuf))}
  | '"' { read_string (Buffer.create 16) lexbuf }
  | '\'' { read_char lexbuf }
  | "type" {TYPE}
  | "use" {USE}
  | "fun" {FUN}
  | "->" {ARROW}
  | "-o" {LIN_ARROW}
  | "let" {LET}
  | "=" {BE}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "case" {CASE}
  | "of" {OF}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MULT}
  | "/" {DIV}
  | ">=" {GEQ}
  | "<=" {LEQ}
  | ">" {GT}
  | "<" {LT}
  | "==" {EQ}
  | "!=" {NEQ}
  | "&&" {AND}
  | "||" {OR}
  | "::" {CONS}
  | "++" {CONCAT}
  | ";" {SEMICOLON}
  | "|>" {PIPE}
  | ":" {COLON}
  | "." {DOT}
  | "?" {QUEST}
  | "," {COMMA}
  | "(" {LPAR}
  | ")" {RPAR}
  | "|" {BAR}
  | "!" {EXCL}
  | "[|" {LARRAY}
  | "|]" {RARRAY}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "_" {WILD}
  | varid {VAR_ID (Lexing.lexeme lexbuf)}
  | constrid {CONSTR_ID (Lexing.lexeme lexbuf)}
  | _ { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  | eof {EOF}
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | escaped   { let ch = List.assoc (Lexing.lexeme_char lexbuf 1) escape_chars in
                Buffer.add_char buf ch; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { failwith ("String is not terminated") }
and read_char =
  parse
  | escaped '\'' { CHAR (List.assoc (Lexing.lexeme_char lexbuf 1) escape_chars) }
  | [^ '\'' '\\'] '\'' { CHAR (Lexing.lexeme_char lexbuf 1) }
  | _ { failwith ("Illegal char literal: " ^ Lexing.lexeme lexbuf) }
  | eof { failwith ("Char is not terminated") }