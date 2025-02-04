README.md

Lisp-Like Language in Haskell

This repository contains a mini interpreter for a Lisp-like language written in Haskell. It showcases several key components of a simple language implementation:

Lexer – Converts raw text into tokens.
Parser – Transforms tokens into an internal AST (here called Sexp).
Pretty Printer – Converts the AST back to a readable form.
Type Checker – Performs basic type checking (including function objects).
Evaluator – Interprets the language constructs and executes them.


## Features:
- Lisp-Style S-Expressions: Expressions are written in parentheses, e.g. (+ 2 3) or if condition thenPart elsePart.
- Typed Primitives: Includes integer and boolean types, plus a mechanism for function objects (fob).
- De Bruijn Indices: Internally, variable references are replaced by integer indices, simplifying evaluation.
- Recursive Bindings: Support for mutually recursive definitions via fix.



## Code Structure:
- Sexp: Defines the core syntax tree for S-expressions.
- Parsing: Uses Parsec to parse the input into Sexp.
- Lexp: An intermediate representation with explicit types and variable names.
- Dexp: A “de-Bruijn” version of the code for easier evaluation.
- Type Checker: Ensures expressions match declared types, reporting errors otherwise.
- Evaluator: Interprets the final Dexp into a Haskell Value.


## Usage:

- Load in GHCi:
- ghci MyLispLike.hs
- Evaluate Expressions:
- You can call evalSexp (read "(+ 1 2)") to evaluate a simple sum.
- Or use run "file.lisp" to parse and evaluate expressions from a file.
- Example
- (fob ((x Num) (y Num))
-   (+ x y))
- Creates a function object that takes two integers and returns their sum.
- (let x 5
-   (let y 10
-     (+ x y)))
- Declares two variables x and y, then sums them.
- (if (= 3 3)
-     42
-     0)
- Returns 42 if 3 = 3 is true, else returns 0.
- Type Checking
- : (if (= x y) 1 0) (-> ? ?)
- You can annotate expressions with : and a type to indicate the expected type.
