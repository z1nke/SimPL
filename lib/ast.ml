type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Lt

type uop =
  | Pos
  | Neg

type expr =
  | Bool of bool
  | Int of int
  | Var of string
  | BinOp of bop * expr * expr
  | UnaryOp of uop * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Lambda of string * expr
  | Apply of expr * expr
  | Pair of expr * expr
  | Car of expr
  | Cdr of expr