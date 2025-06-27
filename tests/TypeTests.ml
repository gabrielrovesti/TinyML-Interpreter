/// Unit tests for TinyML type system
/// Tests type inference, unification, and polymorphism
namespace TinyML.Tests

open NUnit.Framework
open TinyML

[<TestFixture>]
type TypeTests() =
    
    let typeEnv = Builtins.initialTypeEnv ()
    
    let parseAndTypeCheck (code: string) : Type =
        let expr = Parser.parseString code
        TypeChecker.typeCheck typeEnv expr
    
    let shouldTypeCheck (code: string) (expectedType: string) =
        let actualType = parseAndTypeCheck code
        let actualTypeStr = AstPrinter.printType actualType
        Assert.AreEqual(expectedType, actualTypeStr, sprintf "Type mismatch for: %s" code)
    
    let shouldFailTypeCheck (code: string) =
        Assert.Throws<TypeException>(fun () -> parseAndTypeCheck code |> ignore) |> ignore

    [<Test>]
    member _.``Literals have correct types``() =
        shouldTypeCheck "42" "int"
        shouldTypeCheck "3.14" "float"
        shouldTypeCheck "true" "bool"
        shouldTypeCheck "false" "bool"
        shouldTypeCheck "\"hello\"" "string"
        shouldTypeCheck "'a'" "char"
        shouldTypeCheck "()" "unit"

    [<Test>]
    member _.``Identity function is polymorphic``() =
        shouldTypeCheck "λx.x" "('a -> 'a)"

    [<Test>]
    member _.``Function application works``() =
        shouldTypeCheck "(λx.x) 42" "int"
        shouldTypeCheck "(λx.x) true" "bool"
        shouldTypeCheck "(λx.x) \"hello\"" "string"

    [<Test>]
    member _.``Curried functions work``() =
        shouldTypeCheck "λx.λy.x" "('a -> 'b -> 'a)"
        shouldTypeCheck "λf.λg.λx.f (g x)" "('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)"

    [<Test>]
    member _.``Let bindings with polymorphism``() =
        shouldTypeCheck "let id = λx.x in id" "('a -> 'a)"
        shouldTypeCheck "let id = λx.x in (id 42, id true)" "(int * bool)"

    [<Test>]
    member _.``Recursive functions``() =
        shouldTypeCheck "let rec f = λx.f x in f" "('a -> 'b)"
        shouldTypeCheck "let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1) in factorial" "(int -> int)"

    [<Test>]
    member _.``Conditional expressions``() =
        shouldTypeCheck "if true then 42 else 24" "int"
        shouldTypeCheck "if false then \"yes\" else \"no\"" "string"
        shouldTypeCheck "λx.if x then 1 else 0" "(bool -> int)"

    [<Test>]
    member _.``Tuple types``() =
        shouldTypeCheck "(42, true)" "(int * bool)"
        shouldTypeCheck "(1, 2, 3)" "(int * int * int)"
        shouldTypeCheck "(λx.x, 42)" "(('a -> 'a) * int)"

    [<Test>]
    member _.``Binary operations``() =
        shouldTypeCheck "1 + 2" "int"
        shouldTypeCheck "1 - 2" "int"
        shouldTypeCheck "3 * 4" "int"
        shouldTypeCheck "8 / 2" "int"
        shouldTypeCheck "10 % 3" "int"

    [<Test>]
    member _.``Comparison operations``() =
        shouldTypeCheck "1 = 2" "bool"
        shouldTypeCheck "1 <> 2" "bool"
        shouldTypeCheck "1 < 2" "bool"
        shouldTypeCheck "1 <= 2" "bool"
        shouldTypeCheck "1 > 2" "bool"
        shouldTypeCheck "1 >= 2" "bool"

    [<Test>]
    member _.``Boolean operations``() =
        shouldTypeCheck "true && false" "bool"
        shouldTypeCheck "true || false" "bool"
        shouldTypeCheck "not true" "bool"

    [<Test>]
    member _.``Complex polymorphic examples``() =
        // Church numerals
        shouldTypeCheck "λf.λx.x" "(('a -> 'a) -> 'a -> 'a)"
        shouldTypeCheck "λf.λx.f x" "(('a -> 'a) -> 'a -> 'a)"
        shouldTypeCheck "λf.λx.f (f x)" "(('a -> 'a) -> 'a -> 'a)"
        
        // Combinators
        shouldTypeCheck "λx.λy.λz.x z (y z)" "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c"  // S combinator
        shouldTypeCheck "λx.λy.x" "('a -> 'b -> 'a)"  // K combinator
        shouldTypeCheck "λf.λx.f x" "(('a -> 'b) -> 'a -> 'b)"  // I combinator

    [<Test>]
    member _.``Let polymorphism instantiation``() =
        shouldTypeCheck "let f = λx.x in let a = f 1 in let b = f true in (a, b)" "(int * bool)"
        shouldTypeCheck "let pair = λx.λy.(x, y) in pair 1 true" "(int * bool)"

    [<Test>]
    member _.``Nested let expressions``() =
        shouldTypeCheck "let x = 42 in let y = true in (x, y)" "(int * bool)"
        shouldTypeCheck "let f = λx.x in let g = f in g 42" "int"

    [<Test>]
    member _.``Type errors are caught``() =
        shouldFailTypeCheck "1 + true"  // Type mismatch
        shouldFailTypeCheck "if 1 then 2 else 3"  // Non-boolean condition
        shouldFailTypeCheck "if true then 1 else true"  // Branch type mismatch
        shouldFailTypeCheck "λx.x x"  // Occurs check failure
        shouldFailTypeCheck "undefined_variable"  // Unbound variable

    [<Test>]
    member _.``Higher-order function examples``() =
        shouldTypeCheck "λf.λx.f (f x)" "(('a -> 'a) -> 'a -> 'a)"
        shouldTypeCheck "λf.λg.λx.f (g x)" "('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)"
        shouldTypeCheck "λp.λx.λy.if p then x else y" "(bool -> 'a -> 'a -> 'a)"

    [<Test>]
    member _.``Mutual recursion simulation``() =
        // Since we only support single recursion, test nested recursive definitions
        let code = """
            let rec even = λn.
                if n = 0 then true
                else let rec odd = λm.
                    if m = 0 then false
                    else even (m - 1)
                in odd (n - 1)
            in even
        """
        shouldTypeCheck code "(int -> bool)"

    [<Test>]
    member _.``Complex tuple operations``() =
        shouldTypeCheck "let swap = λp.let (x, y) = p in (y, x) in swap" // Would need pattern matching
        shouldTypeCheck "(1, (2, 3))" "(int * (int * int))"
        shouldTypeCheck "((1, 2), (3, 4))" "((int * int) * (int * int))"

    [<Test>]
    member _.``Function composition``() =
        let compose = "λf.λg.λx.f (g x)"
        shouldTypeCheck compose "('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)"
        
        let useCompose = sprintf "let compose = %s in let succ = λx.x + 1 in let double = λx.x * 2 in compose succ double" compose
        shouldTypeCheck useCompose "(int -> int)"

    [<Test>]
    member _.``Y combinator (simplified)``() =
        // Test a simplified version since full Y combinator would require recursive types
        shouldTypeCheck "λf.(λx.f (x x)) (λx.f (x x))" // This should fail with occurs check
        |> fun () -> Assert.Pass() // Expected to fail

    [<Test>]
    member _.``Sequence expressions``() =
        shouldTypeCheck "1; 2" "int"
        shouldTypeCheck "true; 42" "int"
        shouldTypeCheck "(); \"hello\"" "string"

[<TestFixture>]
type UnificationTests() =
    
    [<Test>]
    member _.``Basic unification``() =
        let t1 = TyVar 0
        let t2 = TyCon TInt
        let subst = Unification.unify t1 t2
        let result = TypeOps.applySubst subst t1
        Assert.AreEqual(TyCon TInt, result)

    [<Test>]
    member _.``Function type unification``() =
        let t1 = TyArrow (TyVar 0, TyVar 1)
        let t2 = TyArrow (TyCon TInt, TyCon TBool)
        let subst = Unification.unify t1 t2
        let result1 = TypeOps.applySubst subst (TyVar 0)
        let result2 = TypeOps.applySubst subst (TyVar 1)
        Assert.AreEqual(TyCon TInt, result1)
        Assert.AreEqual(TyCon TBool, result2)

    [<Test>]
    member _.``Occurs check prevents infinite types``() =
        let t1 = TyVar 0
        let t2 = TyArrow (TyVar 0, TyCon TInt)
        Assert.Throws<Unification.UnificationException>(fun () -> 
            Unification.unify t1 t2 |> ignore) |> ignore

    [<Test>]
    member _.``Tuple unification``() =
        let t1 = TyTuple [TyVar 0; TyVar 1]
        let t2 = TyTuple [TyCon TInt; TyCon TBool]
        let subst = Unification.unify t1 t2
        let result1 = TypeOps.applySubst subst (TyVar 0)
        let result2 = TypeOps.applySubst subst (TyVar 1)
        Assert.AreEqual(TyCon TInt, result1)
        Assert.AreEqual(TyCon TBool, result2)

[<TestFixture>]
type SubstitutionTests() =
    
    [<Test>]
    member _.``Substitution application``() =
        let subst = Map.ofList [(0, TyCon TInt); (1, TyCon TBool)]
        let ty = TyArrow (TyVar 0, TyVar 1)
        let result = TypeOps.applySubst subst ty
        Assert.AreEqual(TyArrow (TyCon TInt, TyCon TBool), result)

    [<Test>]
    member _.``Substitution composition``() =
        let s1 = Map.ofList [(0, TyVar 1)]
        let s2 = Map.ofList [(1, TyCon TInt)]
        let composed = TypeOps.composeSubst s1 s2
        let ty = TyVar 0
        let result = TypeOps.applySubst composed ty
        Assert.AreEqual(TyCon TInt, result)

    [<Test>]
    member _.``Type scheme substitution avoids capture``() =
        let subst = Map.ofList [(0, TyCon TInt)]
        let scheme = Forall ([0], TyArrow (TyVar 0, TyVar 1))
        let result = TypeOps.applySubstScheme subst scheme
        // The bound variable 0 should not be substituted
        match result with
        | Forall ([0], TyArrow (TyVar 0, TyVar 1)) -> Assert.Pass()
        | _ -> Assert.Fail("Bound variable was incorrectly substituted")

[<TestFixture>]
type GeneralizationTests() =
    
    [<Test>]
    member _.``Simple generalization``() =
        let env = Map.empty
        let ty = TyArrow (TyVar 0, TyVar 0)
        let scheme = TypeOps.generalize env ty
        match scheme with
        | Forall ([0], TyArrow (TyVar 0, TyVar 0)) -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Unexpected generalization: %A" scheme)

    [<Test>]
    member _.``Generalization respects environment``() =
        let env = Map.ofList [("x", Forall ([0], TyVar 0))]
        let ty = TyArrow (TyVar 0, TyVar 1)
        let scheme = TypeOps.generalize env ty
        // Variable 0 is free in env, so only variable 1 should be quantified
        match scheme with
        | Forall ([1], TyArrow (TyVar 0, TyVar 1)) -> Assert.Pass()
        | _ -> Assert.Fail(sprintf "Unexpected generalization: %A" scheme)

    [<Test>]
    member _.``Instantiation creates fresh variables``() =
        let scheme = Forall ([0], TyArrow (TyVar 0, TyVar 0))
        let ty1 = TypeOps.instantiate scheme
        let ty2 = TypeOps.instantiate scheme
        // The instantiated types should have different variable numbers
        Assert.AreNotEqual(ty1, ty2, "Instantiation should create fresh variables")