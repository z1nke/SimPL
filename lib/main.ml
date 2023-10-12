open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let unbound_var_err = "Unbound variable"
let if_guard_err = "Guard of if must have type bool"
let bop_err = "Binary operator and operand type mismatch"
let uop_err = "Unary operator and operand type mismatch"
let str_lambda_val = "<lambda>"
let apply_err = "The first expression of apply must be lambda"
let does_not_step_err = "Does not step"
let not_pair_err = "Operand is not a pair type"

(** [string_of_val e] converts [e] to string.
    Requires: [e] is a value. *)
let rec string_of_val (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Lambda _ -> str_lambda_val
  | BinOp _ -> failwith "BinOp is not a value"
  | UnaryOp _ -> failwith "UnaryOp is not a value"
  | Let _ -> failwith "Let expression is not a value"
  | If _ -> failwith "If expression is not a value"
  | Var _ -> failwith "Unbound variable"
  | Apply _ -> failwith "Function application is not a value"
  | Pair (e1, e2) -> Format.sprintf "(%s, %s)" (string_of_val e1) (string_of_val e2)
  | Car _ | Cdr _ -> failwith "Car/Cdr expression is not a value"

(** [is_value e] is whether [e] is a value. *)
let rec is_value : expr -> bool = function
  | Bool _ | Int _ | Lambda _ -> true
  | Var _ | BinOp _ | UnaryOp _ | Let _ | If _ | Apply _  -> false
  | Pair (e1, e2) -> is_value e1 && is_value e2
  | Car _ | Cdr _ -> false

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
  | _ -> failwith uop_err

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
  | Pair (e1, e2) ->
    let e1' = subst e1 v x in
    let e2' = subst e2 v x in
    Pair (e1', e2')
  | Car e -> Car (subst e v x)
  | Cdr e -> Cdr (subst e v x)

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
  | Int _ | Bool _ | Lambda _ -> failwith does_not_step_err
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
  | Apply (e1, _) when is_value e1 -> failwith apply_err
  | Apply (e1, e2) when (is_value e2) = false -> Apply(e1, step e2)
  | Apply (e1, e2) -> Apply (step e1, e2)
  | Pair (e1, e2) when is_value e1 && is_value e2 -> failwith does_not_step_err
  | Pair (e1, e2) when is_value e1 -> Pair (e1, step e2)
  | Pair (e1, e2) -> Pair (step e1, e2)
  | Car e -> car_step e
  | Cdr e -> cdr_step e

and car_step = function
| Pair (e, _) -> e
| e when is_value e -> failwith not_pair_err
| e -> Car (step e)

and cdr_step = function
| Pair (_, e) -> e
| e when is_value e -> failwith not_pair_err
| e -> Cdr (step e)


(** [small_eval e] fully evaluates [e] to a value [v]. *)
let rec small_eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> small_eval

let rec eval (e : expr) : expr = match e with
  | Int _ | Bool _ | Lambda _ -> e
  | Var _ -> failwith unbound_var_err
  | BinOp (bop, e1, e2) -> eval_bop bop e1 e2
  | UnaryOp (uop, e1) -> eval_uop uop e1
  | Let (x, e1, e2) -> eval_let x e1 e2
  | If (c, e1, e2) -> eval_if c e1 e2
  | Apply (e1, e2) -> eval_apply e1 e2
  | Pair (e1, e2) -> eval_pair e1 e2
  | Car e -> eval_car e
  | Cdr e -> eval_cdr e

and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Mod, Int a, Int b -> Int (a mod b)
  | Eq, Int a, Int b -> Bool (a = b)
  | Lt, Int a, Int b -> Bool (a < b)
  | _ -> failwith bop_err

and eval_uop uop e = match uop, eval e with
  | Pos, Int a -> Int (a)
  | Neg, Int a -> Int (-a)
  | _ -> failwith uop_err

and eval_let x e1 e2 =
  let v1 = eval e1 in
  let e2' = subst e2 v1 x in
  eval e2'

and eval_if c e1 e2 = let v = eval c in
  if is_value v then
    match v with
    | Bool true -> eval e1
    | Bool false -> eval e2
    | _ -> failwith if_guard_err
  else failwith "Guard of if must be a value" 

and eval_apply e1 e2 =
  let v1 = eval e1 in
  let v2 = eval e2 in
  match v1 with
  | Lambda (x, e) -> let e' = subst e v2 x in eval e'
  | _ -> failwith apply_err

and eval_pair e1 e2 =
  let v1 = eval e1 in
  let v2 = eval e2 in
  Pair (v1, v2)

and eval_car e =
  match eval e with
  | Pair (fst, _) -> fst
  | _ -> failwith not_pair_err

and eval_cdr e =
  match eval e with
  | Pair (_, snd) -> snd
  | _ -> failwith not_pair_err

(** [interp s] interprets [s] by lexing and parsing it.
    evaluating it, and converting the result to a string *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val

let small_interp (s : string) : string =
  s |> parse |> small_eval |> string_of_val