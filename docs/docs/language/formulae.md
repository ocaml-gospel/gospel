---
sidebar_position: 3
---

# Terms and Formulae

Gospel features terms (e.g. `x+1`) and formulae (e.g. `forall i. f i > 0`). This
distinction is made during type checking, and not at the syntax level. In
the following, `expr` stands for a Gospel expression, be it a term or a formula.

## Type expressions

Type expressions follow the OCaml syntax.

```ebnf
typexpr = lname
        | "'" lident
        | "(" typexpr ")"
        | typexpr lpath
        | typexpr, { "," typexpr }
        | (?"? lident ":")? typexpr "->" typexpr
        | typexpr ("*" typexpr)+
lpath = (upath ".")? lident
upath = uident
      | upath "." uident
```

## Expressions

A large fragment of the OCaml syntax is reused for Gospel expressions.

```ebnf
ocaml_expr = constant
           | (upath ".")? ident
           | "(" expr ")"
           | "(" expr ("," expr)+ ")"
           | expr "." "(" expr ")"
           | expr infix_op expr
           | prefix_op expr
           | "not" expr
           | expr expr+
           | "if" expr "then" expr "else" expr
           | "let" pattern "=" expr "in" expr
           | "match" expr ("," expr)* "with" match_cases
           | "fun" binders "->" expr
           | expr ":" typexpr
           | "{" fields "}"
           | "{" expr "with fields "}"
           | "[@...]" expr
binders = lident+ (":" typexpr)?
pattern = "_"
        | lident
        | uname pattern?
        | "()"
        | "(" pattern ")"
        | pattern ("," pattern)+
        | pattern "::" pattern
        | pattern "as lident
        | pattern "|" pattern
        | "{" field_pattern_ (";" field_pattern)* "}"
field_pattern = lname "=" pattern
              | lname
match_cases = "|"? match_case ("|" match_case)*
match_case = pattern "->" expr
fields = field (";" field)*
field = lname "=" expr
      | lname
constant = integer_literal
         | real_literal
         | "true" | "false"
         | "()"
         | "[]"
```


## Gospel-specific expressions

In addition, there is syntax that is specific to Gospel.

```ebnf
expr = ocaml_expr
     | expr "/\" expr
     | expr "\/" expr
     | "old" expr
     | quantifier binders ("," binders)* "." expr
     | expr "[" expr "]"
     | expr "[" expr "->" expr "]"
     | expr "[" expr ".." expr "]"
     | expr "[" ".." expr "]"
     | expr "[" expr ".." "]"
quantifier = "forall" | "exists"
```


Note that `e1[e2]` is part of the OCaml syntax (application of `e1` to a
single-element list `[e2]`) but has a different meaning in Gospel, namely,
access to a sequence element.

There are two operators for logical conjunction, `&&` and `/\`, and two
operators for logical disjunction: `||` and `\/`. A difference between the two,
if any, is tool-specific. For instance, a deductive verification tool may
interpret `A && B` as `A /\ (A -> B)` and a runtime assertion checking tool may
interpret `A && B` as a lazy operator (as in OCaml) and `A /\ B` as a strict
operator.

A noticeable difference w.r.t. the OCaml syntax is that infix operators can be
chained in Gospel. One can write `0 <= n < 100`, for instance, and it is
interpreted as `0 <= n /\ n < 100`.
