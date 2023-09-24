open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] converts [e] to string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | BinOp _ -> failwith "BinOp is not a value"
  | UnaryOp _ -> failwith "UnaryOp is not a value"

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Bool _ -> true
  | Int _ -> true
  | BinOp _ -> false
  | UnaryOp _ -> false

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
  | _ -> failwith "Invalid binary operator step"

(** [step_uop uop v] implements the primitive operation [uop v].
    Requires: [v] is a value. *)
let step_uop uop v =
  match uop, v with
  | Pos, _ -> v
  | Neg, Int a -> Int (-a)
  | _ -> failwith "Invalid unary operator step"

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Int _ -> failwith "Does not step"
  | Bool _ -> failwith "Does not step"
  | BinOp (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | BinOp (bop, e1, e2) when is_value e1 -> BinOp (bop, e1, step e2)
  | BinOp (bop, e1, e2) -> BinOp (bop, step e1, e2)
  | UnaryOp (uop, e) when is_value e -> step_uop uop e
  | UnaryOp (uop, e) -> UnaryOp(uop, step e)

(** [eval e] fully evaluates [e] to a value [v]. *)
let rec eval (e : expr) : expr =
  if is_value e then e else
    e |> step |> eval

(** [interp s] interprets [s] by lexing and parsing it.
    evaluating it, and converting the result to a string *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val