/// Main entry point and REPL for TinyML interpreter
/// Provides interactive environment for TinyML programming
namespace TinyML

open System
open System.IO
open TinyML

/// REPL (Read-Eval-Print Loop) state and commands
type ReplCommand =
    | Eval of string
    | TypeOf of string
    | Load of string
    | Save of string
    | Help
    | Quit
    | Reset
    | Stats
    | Debug of bool

type ReplState = {
    TypeEnv: TypeEnvironment
    EvalEnv: Environment
    History: (string * Type * Value) list
    DebugMode: bool
    TotalStats: EvaluationStats
}

/// REPL implementation
module Repl =
    
    /// Create initial REPL state
    let createState () : ReplState =
        {
            TypeEnv = Builtins.initialTypeEnv ()
            EvalEnv = Builtins.initialEvalEnv ()
            History = []
            DebugMode = false
            TotalStats = Evaluator.createStats ()
        }

    /// Parse REPL command from input
    let parseCommand (input: string) : ReplCommand =
        let trimmed = input.Trim()
        if trimmed.StartsWith(":t ") then
            TypeOf (trimmed.Substring(3))
        elif trimmed.StartsWith(":load ") then
            Load (trimmed.Substring(6))
        elif trimmed.StartsWith(":save ") then
            Save (trimmed.Substring(6))
        elif trimmed = ":help" || trimmed = ":h" then
            Help
        elif trimmed = ":quit" || trimmed = ":q" then
            Quit
        elif trimmed = ":reset" then
            Reset
        elif trimmed = ":stats" then
            Stats
        elif trimmed.StartsWith(":debug ") then
            let flag = trimmed.Substring(7).ToLower()
            Debug (flag = "on" || flag = "true")
        elif trimmed = ":debug" then
            Debug true
        else
            Eval input

    /// Print help information
    let printHelp () =
        printfn """
TinyML Interactive Environment

Commands:
  <expression>      Evaluate expression
  :t <expression>   Show type of expression
  :load <file>      Load and evaluate file
  :save <file>      Save current session to file
  :help, :h         Show this help
  :quit, :q         Exit REPL
  :reset            Reset environment
  :stats            Show evaluation statistics
  :debug [on|off]   Toggle debug mode

Examples:
  let id = λx.x;;
  let compose = λf.λg.λx.f (g x);;
  let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1);;
  factorial 5;;
  :t factorial;;

Language Features:
  - Lambda calculus: λx.e
  - Let bindings: let x = e1 in e2
  - Recursion: let rec f = λx.e1 in e2
  - Conditionals: if e1 then e2 else e3
  - Tuples: (e1, e2, ..., en)
  - Arithmetic: +, -, *, /, %
  - Comparison: =, <>, <, <=, >, >=
  - Boolean: &&, ||, not
  - Literals: integers, floats, booleans, strings, chars, unit
"""

    /// Print statistics
    let printStats (stats: EvaluationStats) =
        printfn """
Evaluation Statistics:
  Total steps: %d
  Max stack depth: %d
  Function calls: %d
  Recursive calls: %d
  Recursion ratio: %.2f%%
""" 
        stats.StepCount 
        stats.MaxStackDepth 
        stats.FunctionCalls 
        stats.RecursiveCalls
        (if stats.FunctionCalls > 0 then float stats.RecursiveCalls / float stats.FunctionCalls * 100.0 else 0.0)

    /// Process single REPL command
    let processCommand (state: ReplState) (command: ReplCommand) : ReplState * bool =
        match command with
        | Eval input ->
            try
                if String.IsNullOrWhiteSpace(input) then
                    (state, false)
                else
                    // Parse expression
                    let expr = Parser.parseString input
                    
                    if state.DebugMode then
                        printfn "Parsed AST: %s" (AstPrinter.printExpr expr)
                    
                    // Type check
                    let inferredType = TypeChecker.typeCheck state.TypeEnv expr
                    
                    if state.DebugMode then
                        printfn "Inferred type: %s" (AstPrinter.printType inferredType)
                    
                    // Evaluate
                    let (result, stats) = Evaluator.evaluateWithStats state.EvalEnv expr
                    
                    if state.DebugMode then
                        printfn "Evaluation steps: %d" stats.StepCount
                    
                    // Print result
                    printfn "- : %s = %s" 
                        (AstPrinter.printType inferredType)
                        (AstPrinter.printValue result)
                    
                    // Update state
                    let newHistory = (input, inferredType, result) :: state.History
                    let newStats = {
                        StepCount = state.TotalStats.StepCount + stats.StepCount
                        MaxStackDepth = max state.TotalStats.MaxStackDepth stats.MaxStackDepth
                        FunctionCalls = state.TotalStats.FunctionCalls + stats.FunctionCalls
                        RecursiveCalls = state.TotalStats.RecursiveCalls + stats.RecursiveCalls
                    }
                    
                    ({ state with History = newHistory; TotalStats = newStats }, false)
            
            with
            | LexerException (msg, pos) ->
                printfn "Lexer error at line %d, column %d: %s" pos.Line pos.Column msg
                (state, false)
            | ParseException (msg, _) ->
                printfn "Parse error: %s" msg
                (state, false)
            | TypeException error ->
                printfn "Type error: %s" (TypeCheckUtils.printTypeError error)
                (state, false)
            | EvaluationException error ->
                printfn "Runtime error: %s" (EvalUtils.printEvaluationError error)
                (state, false)
            | ex ->
                printfn "Unexpected error: %s" ex.Message
                (state, false)

        | TypeOf input ->
            try
                let expr = Parser.parseString input
                let inferredType = TypeChecker.typeCheck state.TypeEnv expr
                printfn "%s : %s" input (AstPrinter.printType inferredType)
                (state, false)
            with
            | ex ->
                printfn "Error: %s" ex.Message
                (state, false)

        | Load filename ->
            try
                if File.Exists(filename) then
                    let content = File.ReadAllText(filename)
                    printfn "Loading %s..." filename
                    let expr = Parser.parseString content
                    let inferredType = TypeChecker.typeCheck state.TypeEnv expr
                    let (result, stats) = Evaluator.evaluateWithStats state.EvalEnv expr
                    printfn "Loaded successfully."
                    printfn "- : %s = %s" 
                        (AstPrinter.printType inferredType)
                        (AstPrinter.printValue result)
                    
                    let newHistory = (content, inferredType, result) :: state.History
                    let newStats = {
                        StepCount = state.TotalStats.StepCount + stats.StepCount
                        MaxStackDepth = max state.TotalStats.MaxStackDepth stats.MaxStackDepth
                        FunctionCalls = state.TotalStats.FunctionCalls + stats.FunctionCalls
                        RecursiveCalls = state.TotalStats.RecursiveCalls + stats.RecursiveCalls
                    }
                    
                    ({ state with History = newHistory; TotalStats = newStats }, false)
                else
                    printfn "File not found: %s" filename
                    (state, false)
            with
            | ex ->
                printfn "Error loading file: %s" ex.Message
                (state, false)

        | Save filename ->
            try
                let content = 
                    state.History
                    |> List.rev
                    |> List.map (fun (expr, _, _) -> expr)
                    |> String.concat "\n\n"
                File.WriteAllText(filename, content)
                printfn "Session saved to %s" filename
                (state, false)
            with
            | ex ->
                printfn "Error saving file: %s" ex.Message
                (state, false)

        | Help ->
            printHelp ()
            (state, false)

        | Quit ->
            printfn "Goodbye!"
            (state, true)

        | Reset ->
            printfn "Environment reset."
            (createState (), false)

        | Stats ->
            printStats state.TotalStats
            (state, false)

        | Debug flag ->
            printfn "Debug mode: %s" (if flag then "ON" else "OFF")
            ({ state with DebugMode = flag }, false)

    /// Main REPL loop
    let rec replLoop (state: ReplState) =
        printf "TinyML> "
        let input = Console.ReadLine()
        
        if input = null then
            // EOF (Ctrl+D)
            printfn "\nGoodbye!"
        else
            let command = parseCommand input
            let (newState, shouldExit) = processCommand state command
            
            if not shouldExit then
                replLoop newState

    /// Start REPL
    let start () =
        printfn "TinyML Interactive Environment v1.0"
        printfn "Type :help for help, :quit to exit"
        printfn ""
        
        let initialState = createState ()
        replLoop initialState

/// File execution utilities
module FileExecution =
    
    /// Execute TinyML file
    let executeFile (filename: string) (verbose: bool) : unit =
        try
            if not (File.Exists(filename)) then
                printfn "Error: File not found: %s" filename
                exit 1
            
            let content = File.ReadAllText(filename)
            
            if verbose then
                printfn "Parsing %s..." filename
            
            let expr = Parser.parseString content
            
            if verbose then
                printfn "AST: %s" (AstPrinter.printExpr expr)
                printfn "Type checking..."
            
            let typeEnv = Builtins.initialTypeEnv ()
            let inferredType = TypeChecker.typeCheck typeEnv expr
            
            if verbose then
                printfn "Type: %s" (AstPrinter.printType inferredType)
                printfn "Evaluating..."
            
            let evalEnv = Builtins.initialEvalEnv ()
            let (result, stats) = Evaluator.evaluateWithStats evalEnv expr
            
            printfn "Result: %s" (AstPrinter.printValue result)
            
            if verbose then
                printfn "Steps: %d" stats.StepCount
                printfn "Max depth: %d" stats.MaxStackDepth
        
        with
        | LexerException (msg, pos) ->
            printfn "Lexer error in %s at line %d, column %d: %s" filename pos.Line pos.Column msg
            exit 1
        | ParseException (msg, _) ->
            printfn "Parse error in %s: %s" filename msg
            exit 1
        | TypeException error ->
            printfn "Type error in %s: %s" filename (TypeCheckUtils.printTypeError error)
            exit 1
        | EvaluationException error ->
            printfn "Runtime error in %s: %s" filename (EvalUtils.printEvaluationError error)
            exit 1
        | ex ->
            printfn "Unexpected error in %s: %s" filename ex.Message
            exit 1

/// Command line argument parsing
module CommandLine =
    
    type Options = {
        Interactive: bool
        Verbose: bool
        Files: string list
        TypeCheck: bool
        ParseOnly: bool
        Optimize: bool
    }

    let defaultOptions = {
        Interactive = false
        Verbose = false
        Files = []
        TypeCheck = false
        ParseOnly = false
        Optimize = false
    }

    let rec parseArgs (args: string list) (options: Options) : Options =
        match args with
        | [] -> options
        | "-i" :: rest | "--interactive" :: rest ->
            parseArgs rest { options with Interactive = true }
        | "-v" :: rest | "--verbose" :: rest ->
            parseArgs rest { options with Verbose = true }
        | "-t" :: rest | "--typecheck" :: rest ->
            parseArgs rest { options with TypeCheck = true }
        | "-p" :: rest | "--parse" :: rest ->
            parseArgs rest { options with ParseOnly = true }
        | "-O" :: rest | "--optimize" :: rest ->
            parseArgs rest { options with Optimize = true }
        | "-h" :: rest | "--help" :: rest ->
            printfn """
TinyML Interpreter

Usage: TinyML [options] [files...]

Options:
  -i, --interactive   Start interactive REPL
  -v, --verbose       Verbose output
  -t, --typecheck     Type check only (don't evaluate)
  -p, --parse         Parse only (don't type check or evaluate)
  -O, --optimize      Enable optimizations
  -h, --help          Show this help

Examples:
  TinyML -i                    # Start REPL
  TinyML program.ml            # Execute file
  TinyML -v program.ml         # Execute with verbose output
  TinyML -t *.ml               # Type check all .ml files
"""
            exit 0
        | file :: rest ->
            parseArgs rest { options with Files = file :: options.Files }

    let run (options: Options) : unit =
        match options.Files with
        | [] ->
            // No files specified, start REPL
            Repl.start ()
        | files ->
            let processFile (file: string) =
                if options.ParseOnly then
                    try
                        let content = File.ReadAllText(file)
                        let expr = Parser.parseString content
                        printfn "%s: Parse successful" file
                        if options.Verbose then
                            printfn "AST: %s" (AstPrinter.printExpr expr)
                    with
                    | ex -> printfn "%s: Parse error: %s" file ex.Message
                
                elif options.TypeCheck then
                    try
                        let content = File.ReadAllText(file)
                        let expr = Parser.parseString content
                        let optimizedExpr = if options.Optimize then 
                                              EvalOptimizations.optimize expr 
                                          else expr
                        let typeEnv = Builtins.initialTypeEnv ()
                        let inferredType = TypeChecker.typeCheck typeEnv optimizedExpr
                        printfn "%s: %s" file (AstPrinter.printType inferredType)
                    with
                    | ex -> printfn "%s: Type error: %s" file ex.Message
                
                else
                    // Full execution
                    FileExecution.executeFile file options.Verbose
            
            // Process each file
            List.rev files |> List.iter processFile
            
            // Start REPL if requested
            if options.Interactive then
                Repl.start ()

/// Main entry point
[<EntryPoint>]
let main (args: string[]) : int =
    try
        let options = CommandLine.parseArgs (List.ofArray args) CommandLine.defaultOptions
        CommandLine.run options
        0
    with
    | ex ->
        printfn "Fatal error: %s" ex.Message
        if ex.StackTrace <> null then
            printfn "Stack trace: %s" ex.StackTrace
        1

/// Additional utility functions for scripting and testing
module Utils =
    
    /// Quick evaluation function for testing
    let eval (code: string) : string =
        try
            let expr = Parser.parseString code
            let typeEnv = Builtins.initialTypeEnv ()
            let evalEnv = Builtins.initialEvalEnv ()
            let ty = TypeChecker.typeCheck typeEnv expr
            let result = Evaluator.evaluate evalEnv expr
            sprintf "%s : %s" (AstPrinter.printValue result) (AstPrinter.printType ty)
        with
        | ex -> sprintf "Error: %s" ex.Message

    /// Quick type checking function
    let typeOf (code: string) : string =
        try
            let expr = Parser.parseString code
            let typeEnv = Builtins.initialTypeEnv ()
            let ty = TypeChecker.typeCheck typeEnv expr
            AstPrinter.printType ty
        with
        | ex -> sprintf "Error: %s" ex.Message

    /// Benchmark evaluation
    let benchmark (code: string) (iterations: int) : TimeSpan =
        let expr = Parser.parseString code
        let typeEnv = Builtins.initialTypeEnv ()
        let evalEnv = Builtins.initialEvalEnv ()
        let _ = TypeChecker.typeCheck typeEnv expr // Ensure it type checks
        
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
        for _ in 1..iterations do
            let _ = Evaluator.evaluate evalEnv expr
            ()
        
        stopwatch.Stop()
        stopwatch.Elapsed

    /// Memory usage estimation (simplified)
    let estimateMemoryUsage (expr: Expr) : int =
        let rec sizeOf = function
            | Literal _ -> 1
            | Variable _ -> 1
            | Lambda (_, body) -> 2 + sizeOf body
            | Application (func, arg) -> 1 + sizeOf func + sizeOf arg
            | Let (_, bindExpr, body) -> 2 + sizeOf bindExpr + sizeOf body
            | LetRec (_, _, bindExpr, body) -> 3 + sizeOf bindExpr + sizeOf body
            | IfThenElse (cond, thenExpr, elseExpr) -> 
                1 + sizeOf cond + sizeOf thenExpr + sizeOf elseExpr
            | Tuple exprs -> 1 + List.sumBy sizeOf exprs
            | BinOp (_, left, right) -> 1 + sizeOf left + sizeOf right
            | Sequence (expr1, expr2) -> 1 + sizeOf expr1 + sizeOf expr2
        sizeOf expr * 8 // Rough estimate: 8 bytes per node
    /// Print memory usage
    let printMemoryUsage (expr: Expr) =
        let usage = estimateMemoryUsage expr
        printfn "Estimated memory usage: %d bytes" usage
    /// Quick test function
    let quickTest (code: string) : unit =
        try
            let expr = Parser.parseString code
            let typeEnv = Builtins.initialTypeEnv ()
            let evalEnv = Builtins.initialEvalEnv ()
            let ty = TypeChecker.typeCheck typeEnv expr
            let result = Evaluator.evaluate evalEnv expr
            printfn "Result: %s : %s" (AstPrinter.printValue result) (AstPrinter.printType ty)
        with
        | ex -> printfn "Error: %s" ex.Message