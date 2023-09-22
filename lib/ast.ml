type bop = 
  | Add
  | Mul


type expr =
  | Int of int
  | BinOp of bop * expr * expr