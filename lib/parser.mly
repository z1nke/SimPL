%token <int> INT
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left TIMES

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; TIMES; e2 = expr { BinOp (Mul, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { BinOp (Add, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;