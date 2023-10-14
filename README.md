# SimPL

A simple program language in OCaml.

## Gramma
```bnf
e ::= x 
    | e1 e2
    | fun x -> e
    | i
    | b
    | e1 bop e2
    | uop e
    | (e1, e2)
    | car e
    | cdr e
    | Left e
    | Right e
    | match e with Left x1 -> e1 | Right x2 -> e2
    | if e1 then e2 else e3
    | let x = e1 in e2

bop ::= +
      | -
      | *
      | /
      | %
      | <
      | ==

uop ::= +
      | -

x ::= <identifiers>
i ::= <integers>
b ::= true
    | false

v ::= fun x -> e
    | i
    | b
    | (v1, v2)
    | Left v
    | Right v
```

## Build
```sh
dune build
```

## Test
```sh
dune test
```

## Reference
- CS3110