{
open Parser
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+


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
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }