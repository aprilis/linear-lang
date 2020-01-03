{
open Lexing
open Parser

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int = '-'? ['0'-'9'] ['0'-'9']*
let idchar = ['a'-'z' 'A'-'Z' '_']
let varid = ['a'-'z' '_'] (idchar | ['0'-'9'])*
let constrid = ['A'-'Z'] (idchar | ['0'-'9'])*

rule read =
  parse
  | white {read lexbuf}
  | newline {new_line lexbuf; read lexbuf}
  | int {INT (int_of_string (Lexing.lexeme lexbuf))}
  | '"' { read_string (Buffer.create 16) lexbuf }
  | "type" {TYPE}
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
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MULT}
  | "&&" {AND}
  | "||" {OR}
  | "::" {CONS}
  | "_" {WILD}
  | varid {VAR_ID (Lexing.lexeme lexbuf)}
  | constrid {CONSTR_ID (Lexing.lexeme lexbuf)}
  | _ { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  | eof {EOF}
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { failwith ("String is not terminated") }