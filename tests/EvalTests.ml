/// Unit tests for TinyML evaluator
/// Tests operational semantics and runtime behavior
namespace TinyML.Tests

open NUnit.Framework
open TinyML

[<TestFixture>]
type EvaluationTests() =
    
    let evalEnv = Builtins.initialEvalEnv ()
    
    let parseAndEvaluate (code: string) : Value =
        let expr = Parser.parseString code
        Evaluator.evaluate evalEnv expr
    
    let shouldEvaluateTo (code: string) (expected: Value) =
        let actual = parseAndEvaluate code
        Assert.AreEqual(expected, actual, sprintf "Evaluation mismatch for: %s" code)
    
    let shouldEvaluateToLiteral (code: string) (expected: Literal) =
        shouldEvaluateTo code (VLiteral expected)
    
    let shouldFailEvaluation (code: string) =
        Assert.Throws<EvaluationException>(fun () -> parseAndEvaluate code |> ignore) |> ignore

    [<Test>]
    member _.``Literals evaluate to themselves``() =
        shouldEvaluateToLiteral "42" (LInt 42)
        shouldEvaluateToLiteral "3.14" (LFloat 3.14)
        shouldEvaluateToLiteral "true" (LBool true)
        shouldEvaluateToLiteral "false" (LBool false)
        shouldEvaluateToLiteral "\"hello\"" (LString "hello")
        shouldEvaluateToLiteral "'a'" (LChar 'a')
        shouldEvaluateToLiteral "()" LUnit

    [<Test>]
    member _.``Identity function works``() =
        shouldEvaluateToLiteral "(λx.x) 42" (LInt 42)
        shouldEvaluateToLiteral "(λx.x) true" (LBool true)
        shouldEvaluateToLiteral "(λx.x) \"test\"" (LString "test")

    [<Test>]
    member _.``Curried function application``() =
        shouldEvaluateToLiteral "(λx.λy.x) 42 true" (LInt 42)
        shouldEvaluateToLiteral "(λx.λy.y) 42 true" (LBool true)
        shouldEvaluateToLiteral "(λx.λy.λz.x) 1 2 3" (LInt 1)

    [<Test>]
    member _.``Arithmetic operations``() =
        shouldEvaluateToLiteral "1 + 2" (LInt 3)
        shouldEvaluateToLiteral "5 - 3" (LInt 2)
        shouldEvaluateToLiteral "4 * 3" (LInt 12)
        shouldEvaluateToLiteral "15 / 3" (LInt 5)
        shouldEvaluateToLiteral "17 % 5" (LInt 2)

    [<Test>]
    member _.``Float arithmetic``() =
        shouldEvaluateToLiteral "1.5 + 2.5" (LFloat 4.0)
        shouldEvaluateToLiteral "5.0 - 2.0" (LFloat 3.0)
        shouldEvaluateToLiteral "2.5 * 4.0" (LFloat 10.0)
        shouldEvaluateToLiteral "7.5 / 2.5" (LFloat 3.0)

    [<Test>]
    member _.``Mixed arithmetic promotion``() =
        shouldEvaluateToLiteral "1 + 2.5" (LFloat 3.5)
        shouldEvaluateToLiteral "2.5 + 1" (LFloat 3.5)
        shouldEvaluateToLiteral "5 * 2.0" (LFloat 10.0)

    [<Test>]
    member _.``Comparison operations``() =
        shouldEvaluateToLiteral "1 = 1" (LBool true)
        shouldEvaluateToLiteral "1 = 2" (LBool false)
        shouldEvaluateToLiteral "1 <> 2" (LBool true)
        shouldEvaluateToLiteral "1 < 2" (LBool true)
        shouldEvaluateToLiteral "2 <= 2" (LBool true)
        shouldEvaluateToLiteral "3 > 2" (LBool true)
        shouldEvaluateToLiteral "2 >= 2" (LBool true)

    [<Test>]
    member _.``Boolean operations``() =
        shouldEvaluateToLiteral "true && true" (LBool true)
        shouldEvaluateToLiteral "true && false" (LBool false)
        shouldEvaluateToLiteral "false || true" (LBool true)
        shouldEvaluateToLiteral "false || false" (LBool false)

    [<Test>]
    member _.``String comparison``() =
        shouldEvaluateToLiteral "\"abc\" = \"abc\"" (LBool true)
        shouldEvaluateToLiteral "\"abc\" = \"def\"" (LBool false)
        shouldEvaluateToLiteral "\"abc\" < \"def\"" (LBool true)
        shouldEvaluateToLiteral "\"xyz\" > \"abc\"" (LBool true)

    [<Test>]
    member _.``Conditional expressions``() =
        shouldEvaluateToLiteral "if true then 1 else 2" (LInt 1)
        shouldEvaluateToLiteral "if false then 1 else 2" (LInt 2)
        shouldEvaluateToLiteral "if 5 > 3 then \"yes\" else \"no\"" (LString "yes")

    [<Test>]
    member _.``Let bindings``() =
        shouldEvaluateToLiteral "let x = 42 in x" (LInt 42)
        shouldEvaluateToLiteral "let x = 10 in let y = 20 in x + y" (LInt 30)
        shouldEvaluateToLiteral "let f = λx.x + 1 in f 41" (LInt 42)

    [<Test>]
    member _.``Lexical scoping``() =
        shouldEvaluateToLiteral "let x = 1 in let f = λy.x + y in let x = 10 in f 5" (LInt 6)
        shouldEvaluateToLiteral "let x = 42 in (λx.x) 24" (LInt 24)

    [<Test>]
    member _.``Recursive functions``() =
        shouldEvaluateToLiteral "let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1) in factorial 5" (LInt 120)
        shouldEvaluateToLiteral "let rec fib = λn.if n <= 1 then n else fib (n-1) + fib (n-2) in fib 7" (LInt 13)

    [<Test>]
    member _.``Higher-order functions``() =
        shouldEvaluateToLiteral "let twice = λf.λx.f (f x) in let succ = λx.x + 1 in twice succ 5" (LInt 7)
        shouldEvaluateToLiteral "let compose = λf.λg.λx.f (g x) in let add1 = λx.x + 1 in let mul2 = λx.x * 2 in compose add1 mul2 5" (LInt 11)

    [<Test>]
    member _.``Tuple construction and values``() =
        let result = parseAndEvaluate "(1, true, \"hello\")"
        match result with
        | VTuple [VLiteral (LInt 1); VLiteral (LBool true); VLiteral (LString "hello")] -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Unexpected tuple result: %A" result)

    [<Test>]
    member _.``Nested tuples``() =
        let result = parseAndEvaluate "((1, 2), (3, 4))"
        match result with
        | VTuple [VTuple [VLiteral (LInt 1); VLiteral (LInt 2)]; 
                  VTuple [VLiteral (LInt 3); VLiteral (LInt 4)]] -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Unexpected nested tuple result: %A" result)

    [<Test>]
    member _.``Sequence expressions``() =
        shouldEvaluateToLiteral "1; 2; 3" (LInt 3)
        shouldEvaluateToLiteral "true; 42" (LInt 42)

    [<Test>]
    member _.``Complex expressions``() =
        let factorial5 = "let rec f = λn.if n = 0 then 1 else n * f (n - 1) in f 5"
        shouldEvaluateToLiteral factorial5 (LInt 120)
        
        let currying = "let add = λx.λy.x + y in let add5 = add 5 in add5 10"
        shouldEvaluateToLiteral currying (LInt 15)
        
        let closure = "let x = 10 in let f = λy.x + y in let x = 20 in f 5"
        shouldEvaluateToLiteral closure (LInt 15)

    [<Test>]
    member _.``Church numerals``() =
        let zero = "λf.λx.x"
        let one = "λf.λx.f x"
        let two = "λf.λx.f (f x)"
        let succ = "λn.λf.λx.f (n f x)"
        
        // Test successor function
        let succOne = sprintf "(%s) (%s)" succ one
        let toInt = "λn.n (λx.x + 1) 0"
        let result = sprintf "(%s) (%s)" toInt succOne
        shouldEvaluateToLiteral result (LInt 2)

    [<Test>]
    member _.``Partial application``() =
        let add = "λx.λy.x + y"
        let add5 = sprintf "(%s) 5" add
        let result = sprintf "(%s) 10" add5
        shouldEvaluateToLiteral result (LInt 15)

    [<Test>]
    member _.``Error cases``() =
        shouldFailEvaluation "1 / 0"  // Division by zero
        shouldFailEvaluation "5 % 0"  // Modulo by zero
        shouldFailEvaluation "undefined_var"  // Unbound variable
        shouldFailEvaluation "42 true"  // Application of non-function

    [<Test>]
    member _.``Stack overflow protection``() =
        let infiniteRecursion = "let rec f = λx.f x in f 42"
        shouldFailEvaluation infiniteRecursion

[<TestFixture>]
type ClosureTests() =
    
    [<Test>]
    member _.``Closures capture environment``() =
        let code = """
            let x = 42 in
            let f = λy.x + y in
            let x = 100 in
            f 8
        """
        let result = Parser.parseString code |> Evaluator.evaluateFresh
        match result with
        | VLiteral (LInt 50) -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Expected 50, got %A" result)

    [<Test>]
    member _.``Recursive closures work correctly``() =
        let code = """
            let rec countdown = λn.
                if n = 0 then 0
                else countdown (n - 1)
            in countdown 5
        """
        let result = Parser.parseString code |> Evaluator.evaluateFresh
        match result with
        | VLiteral (LInt 0) -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Expected 0, got %A" result)

    [<Test>]
    member _.``Mutual recursion simulation``() =
        let code = """
            let rec even = λn.
                if n = 0 then true
                else let rec odd = λm.
                    if m = 0 then false
                    else even (m - 1)
                in odd (n - 1)
            in even 4
        """
        let result = Parser.parseString code |> Evaluator.evaluateFresh
        match result with
        | VLiteral (LBool true) -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Expected true, got %A" result)

[<TestFixture>]
type PerformanceTests() =
    
    [<Test>]
    member _.``Fibonacci performance test``() =
        let code = "let rec fib = λn.if n <= 1 then n else fib (n-1) + fib (n-2) in fib 10"
        let expr = Parser.parseString code
        let evalEnv = Builtins.initialEvalEnv ()
        
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let (result, stats) = Evaluator.evaluateWithStats evalEnv expr
        stopwatch.Stop()
        
        match result with
        | VLiteral (LInt 55) -> 
            Assert.Pass()
            printfn "Fibonacci(10) completed in %d steps, %d function calls" stats.StepCount stats.FunctionCalls
        | _ -> Assert.Fail(sprintf "Expected 55, got %A" result)

    [<Test>]
    member _.``Tail recursion test``() =
        let code = """
            let rec sum_tail = λacc.λn.
                if n = 0 then acc
                else sum_tail (acc + n) (n - 1)
            in sum_tail 0 100
        """
        let expr = Parser.parseString code
        let evalEnv = Builtins.initialEvalEnv ()
        let (result, stats) = Evaluator.evaluateWithStats evalEnv expr
        
        match result with
        | VLiteral (LInt 5050) -> 
            Assert.Pass()
            printfn "Tail recursive sum completed in %d steps" stats.StepCount
        | _ -> Assert.Fail(sprintf "Expected 5050, got %A" result)

[<TestFixture>]
type OptimizationTests() =
    
    [<Test>]
    member _.``Constant folding works``() =
        let expr = Parser.parseString "1 + 2 * 3"
        let optimized = EvalOptimizations.constantFold expr
        
        // Should optimize to some degree, but exact result depends on implementation
        Assert.AreNotEqual(expr, optimized, "Expression should be optimized")

    [<Test>]
    member _.``Dead code elimination works``() =
        let expr = Parser.parseString "let unused = 42 in 24"
        let optimized = EvalOptimizations.eliminateDeadCode expr
        
        match optimized with
        | Literal (LInt 24) -> Assert.Pass()
        | _ -> Assert.Fail("Dead code should be eliminated")

    [<Test>]
    member _.``Optimization preserves semantics``() =
        let codes = [
            "1 + 2 + 3"
            "if true then 42 else 24"
            "let x = 1 + 1 in x * 2"
            "(λx.x) 42"
        ]
        
        let evalEnv = Builtins.initialEvalEnv ()
        
        for code in codes do
            let expr = Parser.parseString code
            let optimized = EvalOptimizations.optimize expr
            
            let result1 = Evaluator.evaluate evalEnv expr
            let result2 = Evaluator.evaluate evalEnv optimized
            
            Assert.AreEqual(result1, result2, sprintf "Optimization changed semantics for: %s" code)

[<TestFixture>]
type ErrorHandlingTests() =
    
    [<Test>]
    member _.``Type errors at runtime are caught``() =
        // These would be caught by type checker in practice,
        // but test runtime type error handling
        Assert.Throws<EvaluationException>(fun () ->
            let value = VLiteral (LString "not a number")
            Evaluator.applyBinOp Add value (VLiteral (LInt 1)) |> ignore
        ) |> ignore

    [<Test>]
    member _.``Stack overflow is detected``() =
        let code = "let rec loop = λx.loop x in loop 42"
        Assert.Throws<EvaluationException>(fun () ->
            Parser.parseString code |> Evaluator.evaluateFresh |> ignore
        ) |> ignore

    [<Test>]
    member _.``Division by zero is caught``() =
        shouldFailEvaluation "10 / 0"
        shouldFailEvaluation "10 % 0"
        shouldFailEvaluation "10.0 / 0.0"
    
    and shouldFailEvaluation (code: string) =
        Assert.Throws<EvaluationException>(fun () ->
            Parser.parseString code |> Evaluator.evaluateFresh |> ignore
        ) |> ignore

[<TestFixture>]
type EvaluationUtilsTests() =
    
    [<Test>]
    member _.``Safe evaluation works``() =
        let evalEnv = Builtins.initialEvalEnv ()
        
        // Simple expression should succeed
        let expr1 = Parser.parseString "1 + 2"
        let result1 = EvalUtils.safeEvaluate evalEnv expr1 1000
        Assert.IsTrue(result1.IsSome)
        
        // Infinite loop should fail/timeout
        let expr2 = Parser.parseString "let rec loop = λx.loop x in loop 42"
        let result2 = EvalUtils.safeEvaluate evalEnv expr2 100
        Assert.IsTrue(result2.IsNone)

    [<Test>]
    member _.``Function arity detection works``() =
        let func1 = Parser.parseString "λx.x" |> Evaluator.evaluateFresh
        let arity1 = EvalUtils.getFunctionArity func1
        Assert.AreEqual(1, arity1)
        
        let func2 = Parser.parseString "λx.λy.x + y" |> Evaluator.evaluateFresh
        let arity2 = EvalUtils.getFunctionArity func2
        Assert.AreEqual(2, arity2)

    [<Test>]
    member _.``Value comparison works``() =
        let val1 = VLiteral (LInt 42)
        let val2 = VLiteral (LInt 42)
        let val3 = VLiteral (LInt 24)
        
        Assert.IsTrue(EvalUtils.compareResults val1 val2)
        Assert.IsFalse(EvalUtils.compareResults val1 val3)
        
        let tuple1 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        let tuple2 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        Assert.IsTrue(EvalUtils.compareResults tuple1 tuple2)
        let tuple3 = VTuple [VLiteral (LInt 1); VLiteral (LBool false)]
        Assert.IsFalse(EvalUtils.compareResults tuple1 tuple3)
    [<Test>]
    member _.``Value to string conversion works``() =
        let val1 = VLiteral (LInt 42)
        let val2 = VLiteral (LString "hello")
        let val3 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        
        Assert.AreEqual("42", EvalUtils.valueToString val1)
        Assert.AreEqual("\"hello\"", EvalUtils.valueToString val2)
        Assert.AreEqual("(1, true)", EvalUtils.valueToString val3)
        
        let complexVal = VClosure ("f", [], evalEnv, VLiteral (LInt 100))
        Assert.AreEqual("<closure: f>", EvalUtils.valueToString complexVal)
    [<Test>]
    member _.``Value equality works``() =
        let val1 = VLiteral (LInt 42)
        let val2 = VLiteral (LInt 42)
        let val3 = VLiteral (LInt 24)
        
        Assert.IsTrue(EvalUtils.valueEquals val1 val2)
        Assert.IsFalse(EvalUtils.valueEquals val1 val3)
        
        let tuple1 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        let tuple2 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        Assert.IsTrue(EvalUtils.valueEquals tuple1 tuple2)
        let tuple3 = VTuple [VLiteral (LInt 1); VLiteral (LBool false)]
        Assert.IsFalse(EvalUtils.valueEquals tuple1 tuple3)
    [<Test>]
    member _.``Value hash code works``() =
        let val1 = VLiteral (LInt 42)
        let val2 = VLiteral (LInt 42)
        let val3 = VLiteral (LInt 24)
        
        Assert.AreEqual(EvalUtils.valueHashCode val1, EvalUtils.valueHashCode val2)
        Assert.AreNotEqual(EvalUtils.valueHashCode val1, EvalUtils.valueHashCode val3)
        
        let tuple1 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        let tuple2 = VTuple [VLiteral (LInt 1); VLiteral (LBool true)]
        Assert.AreEqual(EvalUtils.valueHashCode tuple1, EvalUtils.valueHashCode tuple2)
        let tuple3 = VTuple [VLiteral (LInt 1); VLiteral (LBool false)]
        Assert.AreNotEqual(EvalUtils.valueHashCode tuple1, EvalUtils.valueHashCode tuple3)
        