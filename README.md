# Lisp-Like Interpreter in Haskell

This project is a simple Lisp-like interpreter implemented in Haskell. It parses and evaluates Lisp-style expressions, including support for arithmetic operations, conditionals, function objects, and more.

## Features

- Parses Lisp-like S-expressions
- Supports:
  - Arithmetic operations (`+`, `-`, `*`, `/`)
  - Boolean comparisons (`<`, `>`, `=` and more)
  - Conditionals (`if` statements)
  - User-defined functions (`fob`)
  - Let-bindings and recursion (`let`, `fix`)
- Pretty-printer for S-expressions

## Examples

### Input:
```lisp
(+ 2 (* 3 4))

