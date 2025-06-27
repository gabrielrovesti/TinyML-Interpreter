# TinyML Interpreter - Complete Project Setup

This is a fully functional ML interpreter that demonstrates advanced concepts in programming language theory and implementation, including type inference, operational semantics, and functional programming paradigms.

## 📁 Complete Project Structure

```
TinyML-Interpreter/
├── README.md                    # Comprehensive project documentation
├── LICENSE                      # MIT License
├── .gitignore                   # Git ignore file for F#/.NET
├── TinyML.fsproj               # F# project file with dependencies
│
├── src/                        # Core interpreter implementation
│   ├── Ast.fs                  # Abstract Syntax Tree definitions
│   ├── Types.fs                # Type system and Hindley-Milner operations
│   ├── Environment.fs          # Environment management (Γ and Δ)
│   ├── Lexer.fs                # Lexical analysis and tokenization
│   ├── Parser.fs               # Syntax analysis and AST generation
│   ├── TypeChecker.fs          # Type inference engine
│   ├── Evaluator.fs            # Operational semantics implementation
│   └── Main.fs                 # REPL and command-line interface
│
├── tests/                      # Comprehensive test suite
│   ├── TypeTests.fs            # Type system and inference tests
│   ├── EvalTests.fs            # Evaluation and runtime tests
│   └── ParserTests.fs          # Lexer and parser tests
│
└── examples/                   # Example TinyML programs
    ├── basic.ml                # Basic language constructs
    ├── recursion.ml            # Recursive functions and algorithms
    └── polymorphism.ml         # Advanced polymorphic examples
```

## 🚀 Quick Start Guide

### Prerequisites
- .NET 6.0 or later
- F# compiler (included with .NET)
- Git for version control

### 1. Clone and Setup
```bash
# Clone the repository
git clone https://github.com/yourusername/TinyML-Interpreter.git
cd TinyML-Interpreter

# Restore dependencies and build
dotnet restore
dotnet build

# Run tests to verify everything works
dotnet test
```

### 2. Start the REPL
```bash
dotnet run
```

### 3. Try Some Examples
```fsharp
TinyML> let id = λx.x;;
val id : 'a -> 'a

TinyML> id 42;;
- : int = 42

TinyML> let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1);;
val factorial : int -> int

TinyML> factorial 5;;
- : int = 120

TinyML> :load examples/basic.ml;;
Loading examples/basic.ml...
```

## 🏗️ Architecture Overview

### Core Components

1. **AST (Ast.fs)**: Defines the abstract syntax tree with full ML constructs
2. **Type System (Types.fs)**: Implements Hindley-Milner type inference with:
   - Type unification and substitution
   - Parametric polymorphism
   - Let-polymorphism with generalization/instantiation
3. **Lexer (Lexer.fs)**: Tokenizes source code with proper error handling
4. **Parser (Parser.fs)**: Recursive descent parser with operator precedence
5. **Type Checker (TypeChecker.fs)**: Syntax-directed type inference following formal rules
6. **Evaluator (Evaluator.fs)**: Call-by-value operational semantics with closures
7. **Environment (Environment.fs)**: Manages typing (Γ) and evaluation (Δ) environments

### Language Features

- **Core Lambda Calculus**: λ-abstractions, applications, variables
- **Let Bindings**: `let x = e1 in e2` with polymorphic generalization
- **Recursion**: `let rec f = λx.e1 in e2` with proper recursive closures
- **Data Types**: integers, floats, booleans, strings, characters, unit, tuples
- **Control Flow**: `if-then-else` conditionals
- **Operators**: arithmetic, comparison, logical operations
- **Type Inference**: Complete Hindley-Milner with parametric polymorphism

## 📚 Educational Value

This project demonstrates:

### Theoretical Concepts
- **Type Theory**: Hindley-Milner type system implementation
- **Operational Semantics**: Formal evaluation rules (E-rules)
- **Type Inference**: Constraint generation and unification
- **Lambda Calculus**: Pure functional computation model
- **Polymorphism**: Parametric types and instantiation

### Practical Skills
- **Compiler Construction**: Lexing, parsing, type checking, evaluation
- **F# Programming**: Advanced functional programming techniques
- **Algorithm Implementation**: Unification, substitution, environment management
- **Software Engineering**: Modular design, testing, documentation

## 🧪 Testing Strategy

The project includes comprehensive tests covering:

### Type System Tests (`TypeTests.fs`)
- Basic type inference for literals and functions
- Polymorphic function instantiation
- Let-polymorphism and generalization
- Unification algorithm correctness
- Complex type scenarios and error cases

### Evaluation Tests (`EvalTests.fs`)
- Literal evaluation and basic operations
- Function application and closures
- Recursive function evaluation
- Higher-order function behavior
- Error handling and edge cases

### Parser Tests (`ParserTests.fs`)
- Lexical analysis correctness
- Syntax parsing for all constructs
- Operator precedence and associativity
- Error recovery and reporting
- Round-trip parse/print testing

## 🎓 Academic Integration

### Course Connection
This project directly implements concepts from:
- **Advanced Notes on ML v1.7** by Prof. Alvise Spanò
- ML type system formal specification
- Operational semantics rules (E-Lit, E-Var, E-App, etc.)
- Type inference rules (I-Lit, I-Var, I-App, etc.)

### Research Foundation
Based on foundational papers:
- Damas & Milner: "Principal Type-Schemes for Functional Programs"
- Hindley: "The Principal Type-Scheme of an Object in Combinatory Logic"
- Milner: "A Theory of Type Polymorphism in Programming"

## 🛠️ Development Workflow

### Building and Running
```bash
# Development build
dotnet build

# Run with verbose output
dotnet run -- -v examples/recursion.ml

# Type check only
dotnet run -- -t examples/*.ml

# Start interactive REPL
dotnet run -- -i
```

### Testing
```bash
# Run all tests
dotnet test

# Run specific test class
dotnet test --filter TypeTests

# Verbose test output
dotnet test --verbosity normal
```

### Code Organization
- **Immutable data structures** throughout
- **Functional programming style** with minimal side effects
- **Comprehensive error handling** with custom exceptions
- **Performance considerations** with tail recursion and optimizations

## 📖 Usage Examples

### Basic Programming
```fsharp
# Simple arithmetic
TinyML> 2 + 3 * 4;;
- : int = 14

# Function definition and application
TinyML> let square = λx.x * x in square 5;;
- : int = 25

# Higher-order functions
TinyML> let twice = λf.λx.f (f x) in twice (λx.x + 1) 5;;
- : int = 7
```

### Advanced Features
```fsharp
# Polymorphic functions
TinyML> let compose = λf.λg.λx.f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

# Recursive algorithms
TinyML> let rec gcd = λa.λb.if b = 0 then a else gcd b (a % b) in gcd 48 18;;
- : int = 6

# Church encodings
TinyML> let true_c = λx.λy.x in let false_c = λx.λy.y in true_c "yes" "no";;
- : string = "yes"
```

## 🎯 Learning Outcomes

Students working with this project will:

1. **Understand Type Systems**: Gain deep insight into how type inference works
2. **Master Functional Programming**: Learn advanced FP concepts and techniques
3. **Implement Language Features**: Build complete language constructs from scratch
4. **Apply Formal Methods**: Connect theory to practical implementation
5. **Develop System Thinking**: Understand how language components interact

## 🚀 GitHub Repository Setup

### Repository Configuration
```bash
# Initialize repository
git init
git add .
git commit -m "Initial commit: Complete TinyML interpreter implementation"

# Add remote and push
git remote add origin https://github.com/yourusername/TinyML-Interpreter.git
git branch -M main
git push -u origin main
```

### Recommended GitHub Settings
- **Description**: "Complete ML interpreter with Hindley-Milner type inference (University of Padova Functional Languages course project)"
- **Topics**: `functional-programming`, `ml`, `type-inference`, `interpreter`, `fsharp`, `computer-science`, `university-project`
- **License**: MIT License
- **README**: Automatically detected and displayed

## 📈 Future Enhancements

Potential improvements for advanced students:
- **Pattern Matching**: Add support for algebraic data types
- **Module System**: Implement ML-style modules and functors
- **Garbage Collection**: Add memory management
- **Optimization**: Implement tail call optimization
- **LLVM Backend**: Compile to native code
- **IDE Integration**: Language server protocol support