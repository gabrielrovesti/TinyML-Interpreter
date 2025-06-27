# TinyML Interpreter

A complete ML interpreter implementation in F# following the Hindley-Milner type system and operational semantics as taught in the Functional Languages course at University of Padova. Made for fun and educational purposes, this project showcases the principles of type inference, polymorphism, and functional programming concepts.

## Overview

TinyML is a functional programming language interpreter that implements:
- **Hindley-Milner type system** with parametric polymorphism
- **Type inference** using unification and substitution
- **Operational semantics** with lexical scoping
- **Let-polymorphism** with type schemes and generalization

## Features

### Language Constructs
- **Literals**: integers, floats, booleans, strings, chars, unit
- **Variables**: identifier bindings with type inference
- **Lambda abstractions**: `λx.e` with closures
- **Function application**: with β-reduction
- **Let bindings**: `let x = e1 in e2` with polymorphic generalization
- **Recursive functions**: `let rec f = λx.e1 in e2` with rec-closures
- **Conditionals**: `if e1 then e2 else e3`
- **Tuples**: `(e1, e2, ..., en)` as product types
- **Binary operations**: arithmetic and comparison operators

### Type System
- **Type inference**: Complete Hindley-Milner algorithm
- **Parametric polymorphism**: Generic functions with type variables
- **Unification**: Most general unifier (MGU) computation
- **Type schemes**: Universal quantification for let-polymorphism
- **Substitutions**: Type variable replacement with occurs check

### Interpreter Components
- **Lexer**: Tokenization of source code
- **Parser**: AST generation with precedence handling
- **Type Checker**: Type inference with constraint solving
- **Evaluator**: Operational semantics with environments

## Project Structure

```
TinyML/
├── src/
│   ├── Ast.fs           # Abstract Syntax Tree definitions
│   ├── Types.fs         # Type system and type schemes
│   ├── Lexer.fs         # Lexical analysis
│   ├── Parser.fs        # Syntax analysis
│   ├── TypeChecker.fs   # Type inference engine
│   ├── Evaluator.fs     # Operational semantics
│   ├── Environment.fs   # Environment utilities
│   ├── Unification.fs   # Unification algorithm
│   ├── Substitution.fs  # Type substitutions
│   └── Main.fs          # REPL and entry point
├── tests/
│   ├── TypeTests.fs     # Type inference tests
│   ├── EvalTests.fs     # Evaluation tests
│   └── ParserTests.fs   # Parser tests
├── examples/
│   ├── basic.ml         # Basic examples
│   ├── polymorphism.ml  # Polymorphic functions
│   └── recursion.ml     # Recursive functions
├── TinyML.fsproj        # F# project file
├── README.md            # This file
└── LICENSE              # MIT License
```

## Installation & Usage

### Prerequisites
- .NET 6.0 or later
- F# compiler

### Build & Run
```bash
# Clone the repository
git clone https://github.com/yourusername/TinyML-Interpreter.git
cd TinyML-Interpreter

# Build the project
dotnet build

# Run the REPL
dotnet run

# Run tests
dotnet test
```

### REPL Commands
```
TinyML> let id = λx.x;;
val id : 'a -> 'a

TinyML> let compose = λf.λg.λx.f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

TinyML> let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1);;
val factorial : int -> int

TinyML> factorial 5;;
- : int = 120
```

## Language Grammar

```
e ::= expressions
    | L                    literal
    | x                    variable
    | λx.e                 lambda abstraction
    | e e                  application
    | let x = e in e       let binding
    | let rec f = λx.e in e recursive binding
    | if e then e else e   conditional
    | (e, .., e)          tuple
    | e + e               binary operator

L ::= literals
    | n                   integers
    | m                   floats
    | true | false        booleans
    | "string"            strings
    | 'char'              characters
    | ()                  unit
```

## Type System

### Types
```
τ ::= types
    | c                   type constructor (int, bool, etc.)
    | τ → τ              function type
    | α, β, γ            type variables
    | τ * .. * τ         tuple type

σ ::= ∀α.τ              type schemes
```

### Type Inference Rules
The implementation follows the formal type inference rules with:
- **Generalization**: `gen_Γ(τ) = ∀α.τ where α = ftv(τ) \ ftv(Γ)`
- **Instantiation**: `inst(∀α.τ) = re_α(τ)` with fresh type variables
- **Unification**: `U(τ1, τ2) = θ such that θ(τ1) ≡ θ(τ2)`

## Examples

### Polymorphic Identity Function
```ocaml
let id = λx.x in
let f = id 42 in
let g = id true in
(f, g)
```

### Higher-Order Functions
```ocaml
let map = λf.λlist.
  let rec go = λl.
    if null l then []
    else f (head l) :: go (tail l)
  in go list
in
map (λx.x + 1) [1; 2; 3]
```

### Mutual Recursion
```ocaml
let rec even = λn.
  if n = 0 then true
  else odd (n - 1)
and odd = λn.
  if n = 0 then false
  else even (n - 1)
in
even 42
```

## Implementation Details

### Type Inference Algorithm
1. **Constraint Generation**: Traverse AST generating type equations
2. **Unification**: Solve constraints using most general unifier
3. **Substitution**: Apply solutions to infer principal types
4. **Generalization**: Abstract type variables in let-bindings

### Operational Semantics
- **Closures**: `⟨λx.e, Δ⟩` capture lexical environment
- **Rec-Closures**: `⟨λx.e, f, Δ⟩` enable recursive calls
- **β-Reduction**: Function application via environment extension
- **Lazy Evaluation**: Values computed on-demand

## Testing

The project includes comprehensive tests covering:
- **Type inference**: Polymorphism, unification, generalization
- **Evaluation**: Function application, recursion, conditionals
- **Error handling**: Type errors, unbound variables, parse errors

Run tests with:
```bash
dotnet test --verbosity normal
```

## Contributing

Contributions are welcome! Areas for improvement:
- Pattern matching on algebraic data types
- Module system implementation
- Performance optimizations
- Extended standard library

## References

- [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin Pierce
- [ML for the Working Programmer](https://www.cl.cam.ac.uk/~lp15/MLbook/) by Lawrence Paulson
- [Advanced Notes on ML v1.7](https://github.com/alvisespano/FunctionalLanguages-UniPD) by Prof. Alvise Spanò
- [Principal Type-Schemes for Functional Programs](https://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf) by Damas & Milner

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Prof. Alvise Spanò for the excellent Functional Languages course
- University of Padova Computer Science Department
- The ML community for foundational research

---

**Course**: Functional Languages 2023-2024  
**Institution**: University of Padova  
**Instructor**: Prof. Alvise Spanò  
**Implementation**: F# with Hindley-Milner type system