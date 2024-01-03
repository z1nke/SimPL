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

let not_left_or_right_err = "Operand is not a Left or Right expression"

let not_support_closure = "Not support closure"

(** [string_of_val e] converts [e] to string.
    Requires: [e] is a value. *)
let rec string_of_val (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Lambda _ | Closure _ -> str_lambda_val
  | BinOp _ -> failwith "BinOp is not a value"
  | UnaryOp _ -> failwith "UnaryOp is not a value"
  | Let _ -> failwith "Let expression is not a value"
  | If _ -> failwith "If expression is not a value"
  | Var _ -> failwith "Unbound variable"
  | Apply _ -> failwith "Function application is not a value"
  | Pair (e1, e2) ->
    Format.sprintf "(%s, %s)" (string_of_val e1) (string_of_val e2)
  | Car _ | Cdr _ -> failwith "Car/Cdr expression is not a value"
  | Left e -> Format.sprintf "<left %s>" (string_of_val e)
  | Right e -> Format.sprintf "<right %s>" (string_of_val e)
  | Match _ -> failwith "Match expression is not a value"

let string_of_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Lt -> "<"

let string_of_uop = function Pos -> "+" | Neg -> "-"

let rec string_of_expr (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Var x -> Format.sprintf "(var %s)" x
  | BinOp (bop, e1, e2) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Format.sprintf "(%s%s%s)" s1 (string_of_bop bop) s2
  | UnaryOp (uop, e1) ->
    let s1 = string_of_expr e1 in
    Format.sprintf "(%s%s)" (string_of_uop uop) s1
  | Let (x, e1, e2) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Format.sprintf "(let %s = %s in %s)" x s1 s2
  | If (c, e1, e2) ->
    let sc = string_of_expr c in
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Format.sprintf "(if %s then %s else %s)" sc s1 s2
  | Lambda (x, e1) ->
    let s1 = string_of_expr e1 in
    Format.sprintf "(λ%s. %s)" x s1
  | Closure (x, e1, _) ->
    let s1 = string_of_expr e1 in
    Format.sprintf "(|%s.%s|)" x s1
  | Apply (e1, e2) ->
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Format.sprintf "(%s %s)" s1 s2
  | Pair (e1, e2) ->
    Format.sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Car e1 ->
    let s1 = string_of_expr e1 in
    Format.sprintf "(car %s)" s1
  | Cdr e1 ->
    let s1 = string_of_expr e1 in
    Format.sprintf "(cdr %s)" s1
  | Left e -> Format.sprintf "<left %s>" (string_of_expr e)
  | Right e -> Format.sprintf "<right %s>" (string_of_expr e)
  | Match (m, x1, e1, x2, e2) ->
    let sm = string_of_expr m in
    let s1 = string_of_expr e1 in
    let s2 = string_of_expr e2 in
    Format.sprintf "(match %s with Left %s -> %s | Right %s -> %s)" sm x1 s1 x2
      s2

(** [is_value e] is whether [e] is a value. *)
let rec is_value : expr -> bool = function
  | Bool _ | Int _ | Lambda _ | Closure _ -> true
  | Var _ | BinOp _ | UnaryOp _ | Let _ | If _ | Apply _ -> false
  | Car _ | Cdr _ | Match _ -> false
  | Pair (e1, e2) -> is_value e1 && is_value e2
  | Left e -> is_value e
  | Right e -> is_value e

(** [step_bop bop v1 v2] implements the primitive operation [v1 bop v2].
    Requires: [v1] and [v2] are both values. *)
let step_bop bop v1 v2 =
  match (bop, v1, v2) with
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
  match (uop, v) with
  | Pos, _ -> v
  | Neg, Int a -> Int (-a)
  | _ -> failwith uop_err

let symprefix = "$x"

let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter ;
    symprefix ^ string_of_int !counter

(** [alpha_covert e o n] will replace old variable name [o] to
    new variable name [n] in expression [e] *)
let rec alpha_covert e o n =
  match e with
  | Bool _ | Int _ -> e
  | Var x -> if x = o then Var n else e
  | UnaryOp (uop, e1) -> UnaryOp (uop, alpha_covert e1 o n)
  | BinOp (bop, e1, e2) -> BinOp (bop, alpha_covert e1 o n, alpha_covert e2 o n)
  | Lambda (x, e1) ->
    if String.starts_with x ~prefix:symprefix then Lambda (x, alpha_covert e1 o n)
    else
      let x' = gensym () in
      let new_lambda = Lambda (x', alpha_covert e1 x x') in
      alpha_covert new_lambda o n
  | Closure _ -> failwith not_support_closure
  | Let (x, e1, e2) ->
    if String.starts_with x ~prefix:symprefix then
      Let (x, alpha_covert e1 o n, alpha_covert e2 o n)
    else
      let x' = gensym () in
      let new_let = Let (x', e1, alpha_covert e2 x x') in
      alpha_covert new_let o n
  | If (c, e1, e2) ->
    If (alpha_covert c o n, alpha_covert e1 o n, alpha_covert e2 o n)
  | Apply (e1, e2) -> Apply (alpha_covert e1 o n, alpha_covert e2 o n)
  | Pair (e1, e2) -> Pair (alpha_covert e1 o n, alpha_covert e2 o n)
  | Car e1 -> Car (alpha_covert e1 o n)
  | Cdr e1 -> Cdr (alpha_covert e1 o n)
  | Left e1 -> Left (alpha_covert e1 o n)
  | Right e1 -> Right (alpha_covert e1 o n)
  | Match (m, x1, e1, x2, e2) ->
    if String.starts_with x1 ~prefix:symprefix then
      let m' = alpha_covert m o n in
      let e1' = alpha_covert e1 o n in
      let e2' = alpha_covert e2 o n in
      Match (m', x1, e1', x2, e2')
    else
      let freshx = gensym () in
      let freshy = gensym () in
      let e1' = alpha_covert e1 x1 freshx in
      let e2' = alpha_covert e2 x2 freshy in
      let new_match = Match (m, freshx, e1', freshy, e2') in
      alpha_covert new_match o n

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
    else
      let y' = gensym () in
      let e2' = alpha_covert e2 y y' in
      Let (y', e1', subst e2' v x)
  | If (c, e1, e2) -> If (subst c v x, subst e1 v x, subst e2 v x)
  | Lambda (y, e1) ->
    if x = y then Lambda (x, e1)
    else
      let y' = gensym () in
      let e1' = alpha_covert e1 y y' in
      Lambda (y', subst e1' v x)
  | Closure _ -> failwith not_support_closure
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
  | Left e -> Left (subst e v x)
  | Right e -> Right (subst e v x)
  | Match (e, x1, e1, x2, e2) ->
    let x1' = gensym () in
    let x2' = gensym () in
    let e1' = alpha_covert e1 x1 x1' in
    let e2' = alpha_covert e2 x2 x2' in
    let e' = subst e v x in
    let e1'' = if x = x1 then e1' else subst e1' v x in
    let e2'' = if x = x2 then e2' else subst e2' v x in
    Match (e', x1', e1'', x2', e2'')

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
  | Int _ | Bool _ | Lambda _ | Closure _ -> failwith does_not_step_err
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
  | Apply (e1, e2) when is_value e2 = false -> Apply (e1, step e2)
  | Apply (e1, e2) -> Apply (step e1, e2)
  | Pair (e1, e2) when is_value e1 && is_value e2 -> failwith does_not_step_err
  | Pair (e1, e2) when is_value e1 -> Pair (e1, step e2)
  | Pair (e1, e2) -> Pair (step e1, e2)
  | Car e -> car_step e
  | Cdr e -> cdr_step e
  | Left e -> Left (step e)
  | Right e -> Right (step e)
  | Match (e, x1, e1, x2, e2) -> match_step e x1 e1 x2 e2

and car_step = function
  | Pair (e, _) -> e
  | e when is_value e -> failwith not_pair_err
  | e -> Car (step e)

and cdr_step = function
  | Pair (_, e) -> e
  | e when is_value e -> failwith not_pair_err
  | e -> Cdr (step e)

and match_step e x1 e1 x2 e2 =
  if not (is_value e) then Match (step e, x1, e1, x2, e2)
  else
    match e with
    | Left v when is_value v -> subst e1 v x1
    | Left _ -> Match (e, x1, step e1, x2, e2)
    | Right v when is_value v -> subst e2 v x2
    | Right _ -> Match (e, x1, e1, x2, step e2)
    | _ -> failwith not_left_or_right_err

(** [small_eval e] fully evaluates [e] to a value [v]. *)
let rec small_eval (e : expr) : expr =
  if is_value e then e else e |> step |> small_eval

let rec eval (e : expr) : expr =
  match e with
  | Int _ | Bool _ | Lambda _ -> e
  | Closure _ -> failwith not_support_closure
  | Var _ -> failwith unbound_var_err
  | BinOp (bop, e1, e2) -> eval_bop bop e1 e2
  | UnaryOp (uop, e1) -> eval_uop uop e1
  | Let (x, e1, e2) -> eval_let x e1 e2
  | If (c, e1, e2) -> eval_if c e1 e2
  | Apply (e1, e2) -> eval_apply e1 e2
  | Pair (e1, e2) -> eval_pair e1 e2
  | Car e -> eval_car e
  | Cdr e -> eval_cdr e
  | Left e -> eval_left e
  | Right e -> eval_right e
  | Match (e, x1, e1, x2, e2) -> eval_match e x1 e1 x2 e2

and eval_bop bop e1 e2 =
  match (bop, eval e1, eval e2) with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Mod, Int a, Int b -> Int (a mod b)
  | Eq, Int a, Int b -> Bool (a = b)
  | Lt, Int a, Int b -> Bool (a < b)
  | _ -> failwith bop_err

and eval_uop uop e =
  match (uop, eval e) with
  | Pos, Int a -> Int a
  | Neg, Int a -> Int (-a)
  | _ -> failwith uop_err

and eval_let x e1 e2 =
  let v1 = eval e1 in
  let e2' = subst e2 v1 x in
  eval e2'

and eval_if c e1 e2 =
  let v = eval c in
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
  | Lambda (x, e) ->
    let e' = subst e v2 x in
    eval e'
  | _ -> failwith apply_err

and eval_pair e1 e2 =
  let v1 = eval e1 in
  let v2 = eval e2 in
  Pair (v1, v2)

and eval_car e =
  match eval e with Pair (fst, _) -> fst | _ -> failwith not_pair_err

and eval_cdr e =
  match eval e with Pair (_, snd) -> snd | _ -> failwith not_pair_err

and eval_match e x1 e1 x2 e2 =
  match eval e with
  | Left v -> subst e1 v x1 |> eval
  | Right v -> subst e2 v x2 |> eval
  | _ -> failwith not_left_or_right_err

and eval_left e = if is_value e then Left e else Left (eval e)

and eval_right e = if is_value e then Right e else Right (eval e)

let lookup_env (env : environment) x =
  try Hashtbl.find env x with Not_found -> failwith unbound_var_err

let extend_env (env : environment) x v = Hashtbl.add env x v

let rec env_eval env e : expr =
  match e with
  | Int _ | Bool _ -> e
  | Var x -> lookup_env env x
  | BinOp (bop, e1, e2) ->
    let v1 = env_eval env e1 in
    let v2 = env_eval env e2 in
    step_bop bop v1 v2
  | UnaryOp (uop, e) ->
    let v = env_eval env e in
    step_uop uop v
  | Let (x, e1, e2) ->
    let v1 = env_eval env e1 in
    let new_env = Hashtbl.copy env in
    extend_env new_env x v1 ; env_eval new_env e2
  | If (c, e1, e2) -> (
    let cv = env_eval env c in
    match cv with
    | Bool true -> env_eval env e1
    | Bool false -> env_eval env e2
    | _ -> failwith if_guard_err )
  | Lambda (x, e) -> Closure (x, e, Hashtbl.copy env)
  | Apply (e1, e2) -> (
    match env_eval env e1 with
    | Closure (x, e, closure_env) ->
      let arg = env_eval env e2 in
      extend_env closure_env x arg ;
      env_eval closure_env e
    | _ -> failwith apply_err )
  | Pair (e1, e2) ->
    let v1 = env_eval env e1 in
    let v2 = env_eval env e2 in
    Pair (v1, v2)
  | Car e -> (
    let p = env_eval env e in
    match p with Pair (e1, _) -> e1 | _ -> failwith not_pair_err )
  | Cdr e -> (
    let p = env_eval env e in
    match p with Pair (_, e2) -> e2 | _ -> failwith not_pair_err )
  | Left e ->
    let v = env_eval env e in
    Left v
  | Right e ->
    let v = env_eval env e in
    Right v
  | Match (m, x1, e1, x2, e2) -> (
    let mv = env_eval env m in
    match mv with
    | Left v ->
      let new_env = Hashtbl.copy env in
      extend_env new_env x1 v ; env_eval new_env e1
    | Right v ->
      let new_env = Hashtbl.copy env in
      extend_env new_env x2 v ; env_eval new_env e2
    | _ -> failwith not_left_or_right_err )
  | _ -> e

(** [interp s] interprets [s] by lexing and parsing it.
    evaluating it, and converting the result to a string *)
let interp (s : string) : string = s |> parse |> eval |> string_of_val

let small_interp (s : string) : string =
  s |> parse |> small_eval |> string_of_val

let env_interp (s : string) : string =
  let env = Hashtbl.create 10 in
  s |> parse |> env_eval env |> string_of_val
