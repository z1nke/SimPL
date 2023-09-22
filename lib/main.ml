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
  | Int i -> string_of_int i
  | BinOp _ -> failwith "BinOp is not a value"

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ -> true
  | BinOp _ -> false

(** [step_bop bop v1 v2] implements the primitive operation [v1 bop v2].
    Requires: [v1] and [v2] are both values. *)
let step_bop bop v1 v2 =
  match bop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mul, Int a, Int b -> Int (a * b)
  | _ -> failwith "invalid binary operator step"

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Int _ -> failwith "Does not step"
  | BinOp (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | BinOp (bop, e1, e2) when is_value e1 -> BinOp (bop, e1, step e2)
  | BinOp (bop, e1, e2) -> BinOp (bop, step e1, e2)

(** [eval e] fully evaluates [e] to a value [v]. *)
let rec eval (e : expr) : expr =
  if is_value e then e else
    e |> step |> eval

(** [interp s] interprets [s] by lexing and parsing it.
    evaluating it, and converting the result to a string *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val