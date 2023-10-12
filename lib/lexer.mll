{
open Parser
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+


rule read =
  parse
  | whitespace { read lexbuf }
  | "*" { TIMES }
  | "/" { DIV }
  | "%" { MOD }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "==" { EQ }
  | "<" { LT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { RARROW }
  | "," { COMMA }
  | "car" { CAR }
  | "cdr" { CDR }
  | id { ID (Lexing.lexeme lexbuf)}
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }