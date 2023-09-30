open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let unbound_var_err = "Unbound variable"
let if_guard_err = "Guard of if must have type bool"
let bop_err = "Operator and operand type mismatch"

(** [string_of_val e] converts [e] to string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Lambda _ -> "lambda"
  | BinOp _ -> failwith "BinOp is not a value"
  | UnaryOp _ -> failwith "UnaryOp is not a value"
  | Let _ -> failwith "Let expression is not a value"
  | If _ -> failwith "If expression is not a value"
  | Var _ -> failwith "Unbound variable"
  | Apply _ -> failwith "Function application is not a value"

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Bool _ | Int _ | Lambda _ -> true
  | Var _ | BinOp _ | UnaryOp _ | Let _ | If _ | Apply _  -> false

(** [step_bop bop v1 v2] implements the primitive operation [v1 bop v2].
    Requires: [v1] and [v2] are both values. *)
let step_bop bop v1 v2 =
  match bop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Mod, Int a, Int b -> Int (a mod b)
  | Eq, Int a, Int b -> Bool (a = b)
  | Lt, Int a, Int b -> Bool (a < b)
  | _ -> failwith bop_err

(** [step_uop uop v] implements the primitive operation [uop v].
    Requires: [v] is a value. *)
let step_uop uop v =
  match uop, v with
  | Pos, _ -> v
  | Neg, Int a -> Int (-a)
  | _ -> failwith "Invalid unary operator step"

(** [subst e v x] is e with [v] substituted for [x], that is [e{v/x}]. *)
let rec subst e v x =
  match e with
  | Bool _ | Int _ -> e
  | Var y -> if x = y then v else e
  | UnaryOp (uop, e) -> UnaryOp (uop, subst e v x)
  | BinOp (bop, e1, e2) -> BinOp (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | If (c, e1, e2) ->
    If (subst c v x, subst e1 v x, subst e2 v x)
  | Lambda (y, e1) ->
    if x = y then Lambda (x, e1)
    else Lambda (y, subst e1 v x)
  | Apply (e1, e2) ->
    let e1' = subst e1 v x in
    let e2' = subst e2 v x in
    Apply (e1', e2')

(** [step v1] e1 e2 steps an if expression to either its [then] or [else]
    branch, depending on [v1]. Requires: [v1] is a Boolean value. *)
let step_if v1 e1 e2 =
  if is_value v1 then
    match v1 with
    | Bool true -> e1
    | Bool false -> e2
    | _ -> failwith if_guard_err
  else failwith "Guard of if must be a value"

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Int _ | Bool _ | Lambda _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | BinOp (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | BinOp (bop, e1, e2) when is_value e1 -> BinOp (bop, e1, step e2)
  | BinOp (bop, e1, e2) -> BinOp (bop, step e1, e2)
  | UnaryOp (uop, e) when is_value e -> step_uop uop e
  | UnaryOp (uop, e) -> UnaryOp (uop, step e)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (v, e1, e2) when is_value v -> step_if v e1 e2
  | If (c, e1, e2) -> If (step c, e1, e2)
  | Apply (Lambda (x, e), v) when is_value v -> subst e v x
  | Apply (Lambda (x, e), e2) -> Apply (Lambda (x, e), step e2)
  | Apply (e1, _) when is_value e1 -> failwith "The first expr must be lambda"
  | Apply (e1, e2) when (is_value e2) = false -> Apply(e1, step e2)
  | Apply (e1, e2) -> Apply (step e1, e2)

(** [eval e] fully evaluates [e] to a value [v]. *)
let rec eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval

(** [interp s] interprets [s] by lexing and parsing it.
    evaluating it, and converting the result to a string *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val