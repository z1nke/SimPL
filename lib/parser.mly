%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token EQ
%token LT
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token LET
%token EQUALS
%token IN
%token EOF

%nonassoc IN
%nonassoc ELSE
%left EQ
%left LT
%left PLUS MINUS
%left TIMES DIV MOD

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | TRUE { Bool (true) }
  | FALSE { Bool (false) }
  | x = ID { Var x }
  | e1 = expr; TIMES; e2 = expr { BinOp (Mul, e1, e2) }
  | e1 = expr; DIV; e2 = expr { BinOp (Div, e1, e2) }
  | e1 = expr; MOD; e2 = expr { BinOp (Mod, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { BinOp (Sub, e1, e2) }
  | e1 = expr; EQ; e2 = expr { BinOp (Eq, e1, e2) }
  | e1 = expr; LT; e2 = expr { BinOp (Lt, e1, e2) }
  | PLUS; e = expr { UnaryOp (Pos, e) }
  | MINUS; e = expr { UnaryOp (Neg, e) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2)}
  | IF cond = expr; THEN; e1 = expr; ELSE; e2 = expr { If (cond, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;