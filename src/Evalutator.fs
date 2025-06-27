/// Evaluator for TinyML
/// Implementation of operational semantics with call-by-value evaluation
namespace TinyML

open TinyML

/// Evaluation errors
type EvaluationError =
    | UnboundVariable of string
    | TypeMismatchAtRuntime of string
    | DivisionByZero
    | ApplicationOfNonFunction of Value
    | InvalidBinaryOperation of BinOp * Value * Value
    | RecursionError of string
    | StackOverflow
    | InvalidTupleAccess of int * int

exception EvaluationException of EvaluationError

/// Evaluation statistics for debugging and profiling
type EvaluationStats = {
    StepCount: int
    MaxStackDepth: int
    FunctionCalls: int
    RecursiveCalls: int
}

/// Evaluator implementation following operational semantics rules
module Evaluator =
    
    /// Maximum recursion depth to prevent stack overflow
    let maxRecursionDepth = 1000
    
    /// Create initial evaluation statistics
    let createStats () : EvaluationStats =
        {
            StepCount = 0
            MaxStackDepth = 0
            FunctionCalls = 0
            RecursiveCalls = 0
        }

    /// Update evaluation statistics
    let updateStats (stats: EvaluationStats) (depth: int) (isRecursive: bool) : EvaluationStats =
        {
            StepCount = stats.StepCount + 1
            MaxStackDepth = max stats.MaxStackDepth depth
            FunctionCalls = stats.FunctionCalls + 1
            RecursiveCalls = if isRecursive then stats.RecursiveCalls + 1 else stats.RecursiveCalls
        }

    /// Check if value is true for conditionals
    let isTruthy = function
        | VLiteral (LBool true) -> true
        | VLiteral (LBool false) -> false
        | _ -> failwith "Type error: expected boolean in conditional"

    /// Perform binary operation on values
    let applyBinOp (op: BinOp) (v1: Value) (v2: Value) : Value =
        match op, v1, v2 with
        // Arithmetic operations
        | Add, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LInt (a + b))
        | Sub, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LInt (a - b))
        | Mul, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LInt (a * b))
        | Div, VLiteral (LInt a), VLiteral (LInt b) -> 
            if b = 0 then raise (EvaluationException DivisionByZero)
            else VLiteral (LInt (a / b))
        | Mod, VLiteral (LInt a), VLiteral (LInt b) -> 
            if b = 0 then raise (EvaluationException DivisionByZero)
            else VLiteral (LInt (a % b))
        
        // Float arithmetic
        | Add, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LFloat (a + b))
        | Sub, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LFloat (a - b))
        | Mul, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LFloat (a * b))
        | Div, VLiteral (LFloat a), VLiteral (LFloat b) -> 
            if b = 0.0 then raise (EvaluationException DivisionByZero)
            else VLiteral (LFloat (a / b))
        
        // Mixed int/float arithmetic (int gets promoted to float)
        | Add, VLiteral (LInt a), VLiteral (LFloat b) -> VLiteral (LFloat (float a + b))
        | Add, VLiteral (LFloat a), VLiteral (LInt b) -> VLiteral (LFloat (a + float b))
        | Sub, VLiteral (LInt a), VLiteral (LFloat b) -> VLiteral (LFloat (float a - b))
        | Sub, VLiteral (LFloat a), VLiteral (LInt b) -> VLiteral (LFloat (a - float b))
        | Mul, VLiteral (LInt a), VLiteral (LFloat b) -> VLiteral (LFloat (float a * b))
        | Mul, VLiteral (LFloat a), VLiteral (LInt b) -> VLiteral (LFloat (a * float b))
        | Div, VLiteral (LInt a), VLiteral (LFloat b) -> 
            if b = 0.0 then raise (EvaluationException DivisionByZero)
            else VLiteral (LFloat (float a / b))
        | Div, VLiteral (LFloat a), VLiteral (LInt b) -> 
            if b = 0 then raise (EvaluationException DivisionByZero)
            else VLiteral (LFloat (a / float b))
        
        // Comparison operations
        | Eq, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a = b))
        | Ne, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a <> b))
        | Lt, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a < b))
        | Le, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a <= b))
        | Gt, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a > b))
        | Ge, VLiteral (LInt a), VLiteral (LInt b) -> VLiteral (LBool (a >= b))
        
        // Float comparison
        | Eq, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a = b))
        | Ne, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a <> b))
        | Lt, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a < b))
        | Le, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a <= b))
        | Gt, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a > b))
        | Ge, VLiteral (LFloat a), VLiteral (LFloat b) -> VLiteral (LBool (a >= b))
        
        // Boolean comparison
        | Eq, VLiteral (LBool a), VLiteral (LBool b) -> VLiteral (LBool (a = b))
        | Ne, VLiteral (LBool a), VLiteral (LBool b) -> VLiteral (LBool (a <> b))
        
        // String comparison
        | Eq, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a = b))
        | Ne, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a <> b))
        | Lt, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a < b))
        | Le, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a <= b))
        | Gt, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a > b))
        | Ge, VLiteral (LString a), VLiteral (LString b) -> VLiteral (LBool (a >= b))
        
        // Logical operations
        | And, VLiteral (LBool a), VLiteral (LBool b) -> VLiteral (LBool (a && b))
        | Or, VLiteral (LBool a), VLiteral (LBool b) -> VLiteral (LBool (a || b))
        
        // Unit comparison
        | Eq, VLiteral LUnit, VLiteral LUnit -> VLiteral (LBool true)
        | Ne, VLiteral LUnit, VLiteral LUnit -> VLiteral (LBool false)
        
        // Error cases
        | _ -> raise (EvaluationException (InvalidBinaryOperation (op, v1, v2)))

    /// Main evaluation function following the operational semantics rules
    let rec eval (env: Environment) (expr: Expr) (depth: int) (stats: EvaluationStats) : Value * EvaluationStats =
        if depth > maxRecursionDepth then
            raise (EvaluationException StackOverflow)
        
        let newStats = { stats with StepCount = stats.StepCount + 1; MaxStackDepth = max stats.MaxStackDepth depth }
        
        match expr with
        
        // E-Lit rule: literals evaluate to themselves
        | Literal lit -> (VLiteral lit, newStats)
        
        // E-Var rule: variable lookup
        | Variable x ->
            match EvalEnv.lookup env x with
            | Some value -> (value, newStats)
            | None -> raise (EvaluationException (UnboundVariable x))
        
        // E-Abs rule: lambda abstraction creates closure
        | Lambda (param, body) ->
            let closure = VClosure (param, body, env)
            (closure, newStats)
        
        // E-App and E-App-Rec rules: function application
        | Application (func, arg) ->
            let (funcValue, stats1) = eval env func (depth + 1) newStats
            let (argValue, stats2) = eval env arg (depth + 1) stats1
            
            match funcValue with
            | VClosure (param, body, closureEnv) ->
                // E-App: β-reduction with closure environment
                let extendedEnv = EvalEnv.extend closureEnv param argValue
                let updatedStats = updateStats stats2 depth false
                eval extendedEnv body (depth + 1) updatedStats
            
            | VRecClosure (param, funcName, body, closureEnv) ->
                // E-App-Rec: recursive function application
                let envWithSelf = EvalEnv.extend closureEnv funcName funcValue
                let extendedEnv = EvalEnv.extend envWithSelf param argValue
                let updatedStats = updateStats stats2 depth true
                eval extendedEnv body (depth + 1) updatedStats
            
            | _ -> raise (EvaluationException (ApplicationOfNonFunction funcValue))
        
        // E-Let rule: let binding evaluation
        | Let (var, bindExpr, body) ->
            let (bindValue, stats1) = eval env bindExpr (depth + 1) newStats
            let extendedEnv = EvalEnv.extend env var bindValue
            eval extendedEnv body (depth + 1) stats1
        
        // E-Let-Rec rule: recursive let binding
        | LetRec (funcName, param, bindExpr, body) ->
            let recClosure = VRecClosure (param, funcName, bindExpr, env)
            let extendedEnv = EvalEnv.extend env funcName recClosure
            eval extendedEnv body (depth + 1) newStats
        
        // E-If-True and E-If-False rules: conditional evaluation
        | IfThenElse (cond, thenExpr, elseExpr) ->
            let (condValue, stats1) = eval env cond (depth + 1) newStats
            if isTruthy condValue then
                eval env thenExpr (depth + 1) stats1
            else
                eval env elseExpr (depth + 1) stats1
        
        // E-Tup rule: tuple evaluation
        | Tuple exprs ->
            let rec evalTuple acc stats = function
                | [] -> (VTuple (List.rev acc), stats)
                | expr :: rest ->
                    let (value, newStats) = eval env expr (depth + 1) stats
                    evalTuple (value :: acc) newStats rest
            evalTuple [] newStats exprs
        
        // Binary operation evaluation
        | BinOp (op, left, right) ->
            let (leftValue, stats1) = eval env left (depth + 1) newStats
            let (rightValue, stats2) = eval env right (depth + 1) stats1
            let result = applyBinOp op leftValue rightValue
            (result, stats2)
        
        // Sequence evaluation: evaluate first expression for side effects, return second
        | Sequence (expr1, expr2) ->
            let (_, stats1) = eval env expr1 (depth + 1) newStats
            eval env expr2 (depth + 1) stats1

    /// Evaluate expression with default parameters
    let evaluate (env: Environment) (expr: Expr) : Value =
        let (value, _) = eval env expr 0 (createStats ())
        value

    /// Evaluate with statistics
    let evaluateWithStats (env: Environment) (expr: Expr) : Value * EvaluationStats =
        eval env expr 0 (createStats ())

    /// Evaluate expression with fresh environment
    let evaluateFresh (expr: Expr) : Value =
        evaluate (Builtins.initialEvalEnv ()) expr

/// Built-in function implementations
module BuiltinFunctions =
    
    /// Implementation of built-in arithmetic functions
    let absImpl = function
        | VLiteral (LInt n) -> VLiteral (LInt (abs n))
        | VLiteral (LFloat f) -> VLiteral (LFloat (abs f))
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "abs expects number"))

    let negImpl = function
        | VLiteral (LInt n) -> VLiteral (LInt (-n))
        | VLiteral (LFloat f) -> VLiteral (LFloat (-f))
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "neg expects number"))

    let notImpl = function
        | VLiteral (LBool b) -> VLiteral (LBool (not b))
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "not expects boolean"))

    /// Print functions with side effects
    let printImpl = function
        | VLiteral (LString s) -> 
            printf "%s" s
            VLiteral LUnit
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "print expects string"))

    let printIntImpl = function
        | VLiteral (LInt i) -> 
            printf "%d" i
            VLiteral LUnit
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "print_int expects int"))

    let printBoolImpl = function
        | VLiteral (LBool b) -> 
            printf "%b" b
            VLiteral LUnit
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "print_bool expects bool"))

    let printFloatImpl = function
        | VLiteral (LFloat f) -> 
            printf "%g" f
            VLiteral LUnit
        | v -> raise (EvaluationException (TypeMismatchAtRuntime "print_float expects float"))

    /// Enhanced built-in environment with native function implementations
    let enhancedBuiltinEnv () : Environment =
        let baseEnv = Builtins.initialEvalEnv ()
        
        // Create native function closures
        // Note: In a full implementation, these would be properly integrated
        // For now, we use simplified closures that will be replaced by native calls
        Map.fold Map.add baseEnv (Map.ofList [
            ("abs", VClosure ("x", Variable "x", Map.empty))
            ("neg", VClosure ("x", Variable "x", Map.empty))
            ("not", VClosure ("x", Variable "x", Map.empty))
            ("print", VClosure ("x", Variable "x", Map.empty))
            ("print_int", VClosure ("x", Variable "x", Map.empty))
            ("print_bool", VClosure ("x", Variable "x", Map.empty))
            ("print_float", VClosure ("x", Variable "x", Map.empty))
        ])

/// Evaluation utilities and helpers
module EvalUtils =
    
    /// Pretty print evaluation error
    let printEvaluationError = function
        | UnboundVariable var ->
            sprintf "Unbound variable: %s" var
        | TypeMismatchAtRuntime msg ->
            sprintf "Runtime type error: %s" msg
        | DivisionByZero ->
            "Division by zero"
        | ApplicationOfNonFunction value ->
            sprintf "Attempted to apply non-function value: %s" (AstPrinter.printValue value)
        | InvalidBinaryOperation (op, v1, v2) ->
            sprintf "Invalid binary operation %s on values %s and %s" 
                (AstPrinter.printBinOp op) (AstPrinter.printValue v1) (AstPrinter.printValue v2)
        | RecursionError msg ->
            sprintf "Recursion error: %s" msg
        | StackOverflow ->
            "Stack overflow: maximum recursion depth exceeded"
        | InvalidTupleAccess (index, length) ->
            sprintf "Invalid tuple access: index %d out of bounds for tuple of length %d" index length

    /// Check if evaluation would terminate (simple heuristic)
    let mightTerminate (expr: Expr) : bool =
        let rec check depth = function
            | _ when depth > 10 -> false
            | Literal _ | Variable _ -> true
            | Lambda (_, body) -> check (depth + 1) body
            | Application (func, arg) -> check (depth + 1) func && check (depth + 1) arg
            | Let (_, bindExpr, body) -> check (depth + 1) bindExpr && check (depth + 1) body
            | LetRec (_, _, bindExpr, body) -> check (depth + 1) bindExpr && check (depth + 1) body
            | IfThenElse (cond, thenExpr, elseExpr) -> 
                check (depth + 1) cond && check (depth + 1) thenExpr && check (depth + 1) elseExpr
            | Tuple exprs -> List.forall (check (depth + 1)) exprs
            | BinOp (_, left, right) -> check (depth + 1) left && check (depth + 1) right
            | Sequence (expr1, expr2) -> check (depth + 1) expr1 && check (depth + 1) expr2
        check 0 expr

    /// Safe evaluation with timeout (simplified)
    let safeEvaluate (env: Environment) (expr: Expr) (maxSteps: int) : Value option =
        try
            if mightTerminate expr then
                let (value, stats) = Evaluator.evaluateWithStats env expr
                if stats.StepCount <= maxSteps then
                    Some value
                else
                    None
            else
                None
        with
        | EvaluationException _ -> None

    /// Trace evaluation steps (for debugging)
    type EvaluationTrace = {
        Step: int
        Expression: Expr
        Environment: Environment
        Result: Value option
    }

    let traceEvaluation (env: Environment) (expr: Expr) : EvaluationTrace list =
        let trace = ref []
        let stepCounter = ref 0
        
        let rec tracingEval env expr =
            incr stepCounter
            let step = !stepCounter
            trace := { Step = step; Expression = expr; Environment = env; Result = None } :: !trace
            
            try
                let result = Evaluator.evaluate env expr
                let updatedTrace = { Step = step; Expression = expr; Environment = env; Result = Some result }
                trace := updatedTrace :: (List.tail !trace)
                result
            with
            | e -> 
                let updatedTrace = { Step = step; Expression = expr; Environment = env; Result = None }
                trace := updatedTrace :: (List.tail !trace)
                raise e
        
        try
            let _ = tracingEval env expr
            List.rev !trace
        with
        | _ -> List.rev !trace

    /// Compare evaluation results
    let compareResults (result1: Value) (result2: Value) : bool =
        // Simplified comparison (doesn't handle function equality)
        match result1, result2 with
        | VLiteral l1, VLiteral l2 -> l1 = l2
        | VTuple vs1, VTuple vs2 -> 
            List.length vs1 = List.length vs2 && 
            List.forall2 compareResults vs1 vs2
        | VClosure _, VClosure _ -> false // Functions are not comparable
        | VRecClosure _, VRecClosure _ -> false // Recursive functions are not comparable
        | _ -> false

    /// Extract literal value if possible
    let tryExtractLiteral = function
        | VLiteral lit -> Some lit
        | _ -> None

    /// Check if value is a function
    let isFunction = function
        | VClosure _ | VRecClosure _ -> true
        | _ -> false

    /// Get function arity (for closures)
    let getFunctionArity = function
        | VClosure (_, body, _) ->
            // Count nested lambdas to determine arity
            let rec countParams acc = function
                | Lambda (_, body) -> countParams (acc + 1) body
                | _ -> acc
            1 + countParams 0 body
        | VRecClosure (_, _, body, _) ->
            let rec countParams acc = function
                | Lambda (_, body) -> countParams (acc + 1) body
                | _ -> acc
            1 + countParams 0 body
        | _ -> 0

/// Performance optimization utilities
module EvalOptimizations =
    
    /// Tail call optimization detection
    let isTailCall (expr: Expr) : bool =
        let rec check inTailPosition = function
            | Application (func, _) when inTailPosition -> true
            | Let (_, _, body) -> check inTailPosition body
            | IfThenElse (_, thenExpr, elseExpr) -> 
                check inTailPosition thenExpr || check inTailPosition elseExpr
            | Sequence (_, expr2) -> check inTailPosition expr2
            | _ -> false
        check true expr

    /// Constant folding for expressions
    let rec constantFold = function
        | BinOp (Add, Literal (LInt a), Literal (LInt b)) -> Literal (LInt (a + b))
        | BinOp (Sub, Literal (LInt a), Literal (LInt b)) -> Literal (LInt (a - b))
        | BinOp (Mul, Literal (LInt a), Literal (LInt b)) -> Literal (LInt (a * b))
        | BinOp (Div, Literal (LInt a), Literal (LInt b)) when b <> 0 -> Literal (LInt (a / b))
        | BinOp (And, Literal (LBool true), expr) -> constantFold expr
        | BinOp (And, Literal (LBool false), _) -> Literal (LBool false)
        | BinOp (Or, Literal (LBool true), _) -> Literal (LBool true)
        | BinOp (Or, Literal (LBool false), expr) -> constantFold expr
        | IfThenElse (Literal (LBool true), thenExpr, _) -> constantFold thenExpr
        | IfThenElse (Literal (LBool false), _, elseExpr) -> constantFold elseExpr
        | Application (func, arg) -> Application (constantFold func, constantFold arg)
        | Lambda (param, body) -> Lambda (param, constantFold body)
        | Let (var, bindExpr, body) -> Let (var, constantFold bindExpr, constantFold body)
        | LetRec (func, param, bindExpr, body) -> 
            LetRec (func, param, constantFold bindExpr, constantFold body)
        | IfThenElse (cond, thenExpr, elseExpr) -> 
            IfThenElse (constantFold cond, constantFold thenExpr, constantFold elseExpr)
        | Tuple exprs -> Tuple (List.map constantFold exprs)
        | BinOp (op, left, right) -> BinOp (op, constantFold left, constantFold right)
        | Sequence (expr1, expr2) -> Sequence (constantFold expr1, constantFold expr2)
        | expr -> expr

    /// Dead code elimination
    let rec eliminateDeadCode = function
        | Let (var, bindExpr, body) ->
            let optimizedBody = eliminateDeadCode body
            let freeVars = AstUtils.freeVars optimizedBody
            if Set.contains var freeVars then
                Let (var, eliminateDeadCode bindExpr, optimizedBody)
            else
                optimizedBody
        | Sequence (expr1, expr2) ->
            let optimizedExpr2 = eliminateDeadCode expr2
            // Keep expr1 if it has side effects (simplified check)
            match expr1 with
            | Application _ -> Sequence (eliminateDeadCode expr1, optimizedExpr2)
            | _ -> optimizedExpr2
        | Application (func, arg) -> Application (eliminateDeadCode func, eliminateDeadCode arg)
        | Lambda (param, body) -> Lambda (param, eliminateDeadCode body)
        | LetRec (func, param, bindExpr, body) -> 
            LetRec (func, param, eliminateDeadCode bindExpr, eliminateDeadCode body)
        | IfThenElse (cond, thenExpr, elseExpr) -> 
            IfThenElse (eliminateDeadCode cond, eliminateDeadCode thenExpr, eliminateDeadCode elseExpr)
        | Tuple exprs -> Tuple (List.map eliminateDeadCode exprs)
        | BinOp (op, left, right) -> BinOp (op, eliminateDeadCode left, eliminateDeadCode right)
        | expr -> expr

    /// Full optimization pipeline
    let optimize (expr: Expr) : Expr =
        expr
        |> constantFold
        |> eliminateDeadCode
    |> fun optimizedExpr ->
        if Evaluator.isTailCall optimizedExpr then
            // Apply tail call optimization if applicable
            AstUtils.applyTailCallOptimization optimizedExpr
        else
            optimizedExpr
    /// Apply optimizations to the environment
    let optimizeEnv (env: Environment) : Environment =
        env
        |> Map.map (fun _ value ->
            match value with
            | VClosure (param, body, closureEnv) ->
                let optimizedBody = optimize body
                VClosure (param, optimizedBody, closureEnv)
            | VRecClosure (param, funcName, body, closureEnv) ->
                let optimizedBody = optimize body
                VRecClosure (param, funcName, optimizedBody, closureEnv)
            | _ -> value)
    /// Optimize expression with environment
    let optimizeWithEnv (env: Environment) (expr: Expr) : Expr =
        let optimizedExpr = optimize expr
        optimizeEnv env |> ignore // Apply optimizations to the environment
        optimizedExpr
    /// Optimize expression with fresh environment
    let optimizeFresh (expr: Expr) : Expr =
        let env = Builtins.initialEvalEnv ()
        optimizeWithEnv env expr