{
open Parser
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'?digit+


rule read =
  parse
  | whitespace { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }