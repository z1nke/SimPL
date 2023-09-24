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
  | Int of int
  | BinOp of bop * expr * expr
  | UnaryOp of uop * expr
  | Bool of bool