%{
open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE            // "true"
%token FALSE           // "false"
%token EQ              // "=="
%token LT              // "<"
%token PLUS            // "+"
%token MINUS           // "-"
%token TIMES           // "*"
%token DIV             // "/"
%token MOD             // "%"
%token LPAREN          // "("
%token RPAREN          // ")"
%token IF              // "if"
%token THEN            // "then"
%token ELSE            // "else"
%token LET             // "let"
%token EQUALS          // "="
%token IN              // "in"
%token FUN             // "fun"
%token RARROW          // "->"
%token COMMA           // ","
%token CAR             // "car"
%token CDR             // "cdr"
%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc RARROW
%left EQ
%left LT
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc POS NEG CAR CDR

%start <Ast.expr> prog
%type <Ast.uop> positive
%type <Ast.uop> negative
%type <Ast.expr> sexpr
%type <Ast.bop> binop
%type <Ast.expr> fun_def
%type <Ast.expr> expr

%%

prog:
  | e = expr; EOF { e }
  ;

positive:
  | PLUS { Pos }
  ;

negative:
  | MINUS { Neg }
  ;

sexpr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool (true) }
  | FALSE { Bool (false) }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) }
  ;

%inline binop:
  | PLUS  { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV   { Div }
  | MOD   { Mod }
  | EQ    { Eq  }
  | LT    { Lt  }
  ;

fun_def:
  | RARROW; e = expr { e }
  ;

expr:
  | e = sexpr { e }
  | e1 = expr; op = binop; e2 = expr { BinOp (op, e1, e2) }
  | positive; e = expr %prec POS { UnaryOp (Pos, e) }
  | negative; e = expr %prec NEG { UnaryOp (Neg, e) }
  | CAR; e = expr { Car (e) }
  | CDR; e = expr { Cdr (e) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2)}
  | IF cond = expr; THEN; e1 = expr; ELSE; e2 = expr { If (cond, e1, e2) }
  | f = sexpr; args = sexpr { Apply (f, args) }
  | FUN; x = ID; e = fun_def { Lambda (x, e) }
  ;