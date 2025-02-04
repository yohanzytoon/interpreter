# Lisp-Like Language in Haskell

This repository contains a mini interpreter for a Lisp-like language written in Haskell. It showcases several key components of a simple language implementation:

## Features
- **Lisp-Style S-Expressions**: Expressions are written in parentheses, e.g., `(+ 2 3)` or `(if condition thenPart elsePart)`.
- **Typed Primitives**: Supports integer and boolean types, along with a mechanism for function objects (`fob`).
- **De Bruijn Indices**: Variable references are internally replaced by integer indices to simplify evaluation.
- **Recursive Bindings**: Supports mutually recursive definitions via `fix`.

## Code Structure
- **Sexp**: Defines the core syntax tree for S-expressions.
- **Parsing**: Uses Parsec to parse the input into `Sexp`.
- **Lexp**: An intermediate representation with explicit types and variable names.
- **Dexp**: A de-Bruijn transformed version of the code for easier evaluation.
- **Type Checker**: Ensures expressions match declared types, reporting errors otherwise.
- **Evaluator**: Interprets the final `Dexp` into a Haskell `Value`.

## Usage

### Load in GHCi
```sh
ghci MyLispLike.hs
```

### Evaluate Expressions
You can call:
```haskell
evalSexp (read "(+ 1 2)")
```
To evaluate a simple sum.

Or use:
```sh
run "file.lisp"
```
To parse and evaluate expressions from a file.

### Example Expressions
#### Function Definition
```lisp
(fob ((x Num) (y Num))
   (+ x y))
```
Creates a function object that takes two integers and returns their sum.

#### Variable Declaration
```lisp
(let x 5
  (let y 10
    (+ x y)))
```
Declares two variables `x` and `y`, then sums them.

#### Conditional Expression
```lisp
(if (= 3 3)
    42
    0)
```
Returns `42` if `3 = 3` is true, otherwise returns `0`.

### Type Checking
```lisp
: (if (= x y) 1 0) (-> ? ?)
```
You can annotate expressions with `:` and a type to indicate the expected type.

