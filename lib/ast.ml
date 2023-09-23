type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type uop =
  | Pos
  | Neg

type expr =
  | Int of int
  | BinOp of bop * expr * expr
  | UnaryOp of uop * expr