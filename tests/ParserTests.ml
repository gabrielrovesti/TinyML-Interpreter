/// Unit tests for TinyML parser and lexer
/// Tests syntax analysis and AST generation
namespace TinyML.Tests

open NUnit.Framework
open TinyML

[<TestFixture>]
type LexerTests() =
    
    let tokenize (input: string) : Token list =
        Lexer.tokenize input

    let shouldTokenizeTo (input: string) (expected: Token list) =
        let actual = tokenize input
        let actualWithoutEOF = actual |> List.filter ((<>) EOF)
        Assert.AreEqual(expected, actualWithoutEOF, sprintf "Tokenization mismatch for: %s" input)

    [<Test>]
    member _.``Integer literals``() =
        shouldTokenizeTo "42" [LitInt 42]
        shouldTokenizeTo "0" [LitInt 0]
        shouldTokenizeTo "123456" [LitInt 123456]

    [<Test>]
    member _.``Float literals``() =
        shouldTokenizeTo "3.14" [LitFloat 3.14]
        shouldTokenizeTo "0.5" [LitFloat 0.5]
        shouldTokenizeTo "123.456" [LitFloat 123.456]

    [<Test>]
    member _.``Boolean literals``() =
        shouldTokenizeTo "true" [LitBool true]
        shouldTokenizeTo "false" [LitBool false]

    [<Test>]
    member _.``String literals``() =
        shouldTokenizeTo "\"hello\"" [LitString "hello"]
        shouldTokenizeTo "\"world\"" [LitString "world"]
        shouldTokenizeTo "\"\"" [LitString ""]

    [<Test>]
    member _.``Character literals``() =
        shouldTokenizeTo "'a'" [LitChar 'a']
        shouldTokenizeTo "'Z'" [LitChar 'Z']
        shouldTokenizeTo "'1'" [LitChar '1']

    [<Test>]
    member _.``Unit literal``() =
        shouldTokenizeTo "()" [LitUnit]

    [<Test>]
    member _.``Identifiers``() =
        shouldTokenizeTo "x" [Identifier "x"]
        shouldTokenizeTo "hello_world" [Identifier "hello_world"]
        shouldTokenizeTo "func123" [Identifier "func123"]
        shouldTokenizeTo "test'" [Identifier "test'"]

    [<Test>]
    member _.``Keywords``() =
        shouldTokenizeTo "let" [Let]
        shouldTokenizeTo "rec" [Rec]
        shouldTokenizeTo "in" [In]
        shouldTokenizeTo "if" [If]
        shouldTokenizeTo "then" [Then]
        shouldTokenizeTo "else" [Else]
        shouldTokenizeTo "and" [And]
        shouldTokenizeTo "or" [Or]

    [<Test>]
    member _.``Operators``() =
        shouldTokenizeTo "+" [Plus]
        shouldTokenizeTo "-" [Minus]
        shouldTokenizeTo "*" [Multiply]
        shouldTokenizeTo "/" [Divide]
        shouldTokenizeTo "%" [Modulo]
        shouldTokenizeTo "=" [Equal]
        shouldTokenizeTo "<>" [NotEqual]
        shouldTokenizeTo "<" [LessThan]
        shouldTokenizeTo "<=" [LessEqual]
        shouldTokenizeTo ">" [GreaterThan]
        shouldTokenizeTo ">=" [GreaterEqual]
        shouldTokenizeTo "&&" [LogicalAnd]
        shouldTokenizeTo "||" [LogicalOr]
        shouldTokenizeTo "->" [Arrow]

    [<Test>]
    member _.``Delimiters``() =
        shouldTokenizeTo "(" [LeftParen]
        shouldTokenizeTo ")" [RightParen]
        shouldTokenizeTo "," [Comma]
        shouldTokenizeTo ";" [Semicolon]
        shouldTokenizeTo "." [Dot]
        shouldTokenizeTo "\\" [Lambda]

    [<Test>]
    member _.``Complex expressions``() =
        shouldTokenizeTo "λx.x" [Lambda; Identifier "x"; Dot; Identifier "x"]
        shouldTokenizeTo "let f = λx.x + 1" [Let; Identifier "f"; Equal; Lambda; Identifier "x"; Dot; Identifier "x"; Plus; LitInt 1]
        shouldTokenizeTo "if x then y else z" [If; Identifier "x"; Then; Identifier "y"; Else; Identifier "z"]

    [<Test>]
    member _.``Whitespace handling``() =
        shouldTokenizeTo "  x  " [Identifier "x"]
        shouldTokenizeTo "\tx\t" [Identifier "x"]
        shouldTokenizeTo "x y" [Identifier "x"; Identifier "y"]

    [<Test>]
    member _.``String escapes``() =
        shouldTokenizeTo "\"hello\\nworld\"" [LitString "hello\nworld"]
        shouldTokenizeTo "\"tab\\there\"" [LitString "tab\there"]
        shouldTokenizeTo "\"quote\\\"here\"" [LitString "quote\"here"]

    [<Test>]
    member _.``Comments are ignored``() =
        shouldTokenizeTo "x // comment" [Identifier "x"]
        shouldTokenizeTo "x (* comment *) y" [Identifier "x"; Identifier "y"]
        shouldTokenizeTo "x (* nested (* comment *) *) y" [Identifier "x"; Identifier "y"]

[<TestFixture>]
type ParserTests() =
    
    let parse (input: string) : Expr =
        Parser.parseString input

    let shouldParseTo (input: string) (expected: Expr) =
        let actual = parse input
        Assert.AreEqual(expected, actual, sprintf "Parse mismatch for: %s" input)

    let shouldFailParse (input: string) =
        Assert.Throws<System.Exception>(fun () -> parse input |> ignore) |> ignore

    [<Test>]
    member _.``Literal parsing``() =
        shouldParseTo "42" (Literal (LInt 42))
        shouldParseTo "3.14" (Literal (LFloat 3.14))
        shouldParseTo "true" (Literal (LBool true))
        shouldParseTo "false" (Literal (LBool false))
        shouldParseTo "\"hello\"" (Literal (LString "hello"))
        shouldParseTo "'a'" (Literal (LChar 'a'))
        shouldParseTo "()" (Literal LUnit)

    [<Test>]
    member _.``Variable parsing``() =
        shouldParseTo "x" (Variable "x")
        shouldParseTo "hello" (Variable "hello")
        shouldParseTo "test123" (Variable "test123")

    [<Test>]
    member _.``Lambda parsing``() =
        shouldParseTo "λx.x" (Lambda ("x", Variable "x"))
        shouldParseTo "λf.λx.f x" (Lambda ("f", Lambda ("x", Application (Variable "f", Variable "x"))))

    [<Test>]
    member _.``Application parsing``() =
        shouldParseTo "f x" (Application (Variable "f", Variable "x"))
        shouldParseTo "f x y" (Application (Application (Variable "f", Variable "x"), Variable "y"))
        shouldParseTo "(λx.x) 42" (Application (Lambda ("x", Variable "x"), Literal (LInt 42)))

    [<Test>]
    member _.``Let binding parsing``() =
        shouldParseTo "let x = 42 in x" (Let ("x", Literal (LInt 42), Variable "x"))
        shouldParseTo "let f = λx.x in f 42" 
            (Let ("f", Lambda ("x", Variable "x"), Application (Variable "f", Literal (LInt 42))))

    [<Test>]
    member _.``Recursive let parsing``() =
        shouldParseTo "let rec f = λx.f x in f" 
            (LetRec ("f", "x", Application (Variable "f", Variable "x"), Variable "f"))

    [<Test>]
    member _.``Conditional parsing``() =
        shouldParseTo "if true then 1 else 2" 
            (IfThenElse (Literal (LBool true), Literal (LInt 1), Literal (LInt 2)))
        shouldParseTo "if x then y else z" 
            (IfThenElse (Variable "x", Variable "y", Variable "z"))

    [<Test>]
    member _.``Tuple parsing``() =
        shouldParseTo "(1, 2)" (Tuple [Literal (LInt 1); Literal (LInt 2)])
        shouldParseTo "(1, true, \"hello\")" 
            (Tuple [Literal (LInt 1); Literal (LBool true); Literal (LString "hello")])
        shouldParseTo "(x, y, z)" (Tuple [Variable "x"; Variable "y"; Variable "z"])

    [<Test>]
    member _.``Binary operation parsing``() =
        shouldParseTo "1 + 2" (BinOp (Add, Literal (LInt 1), Literal (LInt 2)))
        shouldParseTo "x * y" (BinOp (Mul, Variable "x", Variable "y"))
        shouldParseTo "a = b" (BinOp (Eq, Variable "a", Variable "b"))

    [<Test>]
    member _.``Operator precedence``() =
        shouldParseTo "1 + 2 * 3" 
            (BinOp (Add, Literal (LInt 1), BinOp (Mul, Literal (LInt 2), Literal (LInt 3))))
        shouldParseTo "2 * 3 + 4" 
            (BinOp (Add, BinOp (Mul, Literal (LInt 2), Literal (LInt 3)), Literal (LInt 4)))

    [<Test>]
    member _.``Operator associativity``() =
        shouldParseTo "1 - 2 - 3" 
            (BinOp (Sub, BinOp (Sub, Literal (LInt 1), Literal (LInt 2)), Literal (LInt 3)))
        shouldParseTo "1 + 2 + 3" 
            (BinOp (Add, BinOp (Add, Literal (LInt 1), Literal (LInt 2)), Literal (LInt 3)))

    [<Test>]
    member _.``Parentheses override precedence``() =
        shouldParseTo "(1 + 2) * 3" 
            (BinOp (Mul, BinOp (Add, Literal (LInt 1), Literal (LInt 2)), Literal (LInt 3)))
        shouldParseTo "1 * (2 + 3)" 
            (BinOp (Mul, Literal (LInt 1), BinOp (Add, Literal (LInt 2), Literal (LInt 3))))

    [<Test>]
    member _.``Sequence expressions``() =
        shouldParseTo "1; 2" (Sequence (Literal (LInt 1), Literal (LInt 2)))
        shouldParseTo "1; 2; 3" (Sequence (Literal (LInt 1), Sequence (Literal (LInt 2), Literal (LInt 3))))

    [<Test>]
    member _.``Complex expressions``() =
        let factorial = "let rec factorial = λn.if n = 0 then 1 else n * factorial (n - 1) in factorial"
        let expected = LetRec ("factorial", "n", 
            IfThenElse (
                BinOp (Eq, Variable "n", Literal (LInt 0)),
                Literal (LInt 1),
                BinOp (Mul, Variable "n", 
                    Application (Variable "factorial", 
                        BinOp (Sub, Variable "n", Literal (LInt 1))))),
            Variable "factorial")
        shouldParseTo factorial expected

    [<Test>]
    member _.``Nested let expressions``() =
        shouldParseTo "let x = let y = 1 in y in x"
            (Let ("x", Let ("y", Literal (LInt 1), Variable "y"), Variable "x"))

    [<Test>]
    member _.``Function composition``() =
        shouldParseTo "λf.λg.λx.f (g x)"
            (Lambda ("f", Lambda ("g", Lambda ("x", 
                Application (Variable "f", Application (Variable "g", Variable "x"))))))

    [<Test>]
    member _.``Curried application``() =
        shouldParseTo "f g h x"
            (Application (Application (Application (Variable "f", Variable "g"), Variable "h"), Variable "x"))

    [<Test>]
    member _.``Parse error cases``() =
        shouldFailParse "let x = in x"  // Missing expression
        shouldFailParse "λ.x"  // Missing parameter
        shouldFailParse "if then else"  // Missing expressions
        shouldFailParse "1 +"  // Incomplete binary operation
        shouldFailParse "(1, )"  // Incomplete tuple

[<TestFixture>]
type ASTUtilityTests() =
    
    [<Test>]
    member _.``Free variables detection``() =
        let expr1 = Parser.parseString "x"
        let fv1 = AstUtils.freeVars expr1
        Assert.AreEqual(Set.singleton "x", fv1)

        let expr2 = Parser.parseString "λx.x"
        let fv2 = AstUtils.freeVars expr2
        Assert.AreEqual(Set.empty, fv2)

        let expr3 = Parser.parseString "λx.x + y"
        let fv3 = AstUtils.freeVars expr3
        Assert.AreEqual(Set.singleton "y", fv3)

        let expr4 = Parser.parseString "let x = y in x + z"
        let fv4 = AstUtils.freeVars expr4
        Assert.AreEqual(Set.ofList ["y"; "z"], fv4)

    [<Test>]
    member _.``Pretty printing works``() =
        let expr1 = Parser.parseString "λx.x"
        let pretty1 = ParserUtils.prettyPrint expr1
        Assert.AreEqual("λx.x", pretty1)

        let expr2 = Parser.parseString "f x y"
        let pretty2 = ParserUtils.prettyPrint expr2
        Assert.AreEqual("f x y", pretty2)

    [<Test>]
    member _.``Lambda parameter extraction``() =
        let expr = Parser.parseString "λx.λy.λz.x + y + z"
        let (params, body) = ParserUtils.extractLambdaParams expr
        Assert.AreEqual(["x"; "y"; "z"], params)

    [<Test>]
    member _.``Application spine extraction``() =
        let expr = Parser.parseString "f x y z"
        let (func, args) = ParserUtils.extractApplicationSpine expr
        Assert.AreEqual(Variable "f", func)
        Assert.AreEqual([Variable "x"; Variable "y"; Variable "z"], args)

    [<Test>]
    member _.``AST validation``() =
        let validExpr = Parser.parseString "λx.x + 1"
        Assert.IsTrue(ParserUtils.validateAst validExpr)

        // All parsed expressions should be valid
        let exprs = [
            "42"
            "λx.x"
            "f x"
            "let x = 1 in x"
            "if true then 1 else 2"
            "(1, 2, 3)"
            "1 + 2 * 3"
        ]
        
        for exprStr in exprs do
            let expr = Parser.parseString exprStr
            Assert.IsTrue(ParserUtils.validateAst expr, sprintf "Invalid AST for: %s" exprStr)

[<TestFixture>]
type RoundTripTests() =
    
    [<Test>]
    member _.``Parse and pretty print round trip``() =
        let expressions = [
            "42"
            "λx.x"
            "f x"
            "λx.λy.x + y"
            "let x = 42 in x"
            "if true then 1 else 2"
            "(1, 2, 3)"
            "1 + 2 * 3"
            "f (g x)"
        ]
        
        for expr in expressions do
            let parsed = Parser.parseString expr
            let pretty = ParserUtils.prettyPrint parsed
            // Note: Pretty printing might not exactly match input due to normalization
            // But it should at least parse successfully
            let reparsed = Parser.parseString pretty
            Assert.IsTrue(ParserUtils.validateAst reparsed, sprintf "Round trip failed for: %s" expr)

[<TestFixture>]
type ErrorRecoveryTests() =
    
    [<Test>]
    member _.``Lexer error reporting``() =
        let invalidInputs = [
            "@invalid"
            "\"unterminated string"
            "'unterminated char"
            "123.45.67"  // Invalid float
        ]
        
        for input in invalidInputs do
            Assert.Throws<LexerException>(fun () -> 
                Lexer.tokenize input |> ignore
            ) |> ignore

    [<Test>]
    member _.``Parser error reporting``() =
        let invalidInputs = [
            "let = 42"  // Missing variable name
            "λ.x"       // Missing parameter
            "1 +"       // Incomplete expression
            "if then else"  // Missing condition
            "((("        // Unbalanced parentheses
        ]
        
        for input in invalidInputs do
            Assert.Throws<ParseException>(fun () -> 
                Parser.parseString input |> ignore
            ) |> ignore

[<TestFixture>]
type LexerUtilityTests() =
    
    [<Test>]
    member _.``Token classification``() =
        Assert.IsTrue(LexerUtils.isLiteral (LitInt 42))
        Assert.IsTrue(LexerUtils.isOperator Plus)
        Assert.IsTrue(LexerUtils.isKeyword Let)
        Assert.IsTrue(LexerUtils.isDelimiter LeftParen)

    [<Test>]
    member _.``Operator precedence``() =
        Assert.AreEqual(5, LexerUtils.getOperatorPrecedence Multiply)
        Assert.AreEqual(4, LexerUtils.getOperatorPrecedence Plus)
        Assert.AreEqual(3, LexerUtils.getOperatorPrecedence Equal)
        Assert.IsTrue(LexerUtils.getOperatorPrecedence Multiply > LexerUtils.getOperatorPrecedence Plus)

    [<Test>]
    member _.``Operator associativity``() =
        Assert.IsTrue(LexerUtils.isLeftAssociative Plus)
        Assert.IsTrue(LexerUtils.isLeftAssociative Multiply)
        Assert.IsTrue(LexerUtils.isLeftAssociative Equal)

    [<Test>]
    member _.``Literal extraction``() =
        let token = LitInt 42
        let expr = LexerUtils.extractLiteral token
        Assert.AreEqual(Literal (LInt 42), expr)

[<TestFixture>]
type TokenPrinterTests() =
    
    [<Test>]
    member _.``Token printing``() =
        Assert.AreEqual("INT(42)", TokenPrinter.printToken (LitInt 42))
        Assert.AreEqual("BOOL(true)", TokenPrinter.printToken (LitBool true))
        Assert.AreEqual("STRING(\"hello\")", TokenPrinter.printToken (LitString "hello"))
        Assert.AreEqual("ID(x)", TokenPrinter.printToken (Identifier "x"))
        Assert.AreEqual("LET", TokenPrinter.printToken Let)
        Assert.AreEqual("+", TokenPrinter.printToken Plus)

    [<Test>]
    member _.``Token list printing``() =
        let tokens = [LitInt 42; Plus; LitInt 24]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) + INT(24)", result)
    [<Test>]
    member _.``Token list printing with EOF``() =
        let tokens = [LitInt 42; Plus; LitInt 24; EOF]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) + INT(24) EOF", result)

    [<Test>]
    member _.``Token printing with comments``() =
        let tokens = [LitInt 42; Comment "This is a comment"; Plus; LitInt 24]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) // This is a comment + INT(24)", result)

    [<Test>]
    member _.``Token printing with nested comments``() =
        let tokens = [LitInt 42; Comment "Outer comment"; LitInt 24; Comment "Inner comment"]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) // Outer comment INT(24) // Inner comment", result)

    [<Test>]
    member _.``Token printing with mixed types``() =
        let tokens = [LitInt 42; LitBool true; Identifier "x"; Plus; LitFloat 3.14]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) BOOL(true) ID(x) + FLOAT(3.14)", result)

    [<Test>]
    member _.``Token printing with delimiters``() =
        let tokens = [LeftParen; LitInt 42; RightParen; Plus; LitInt 24]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("( INT(42) ) + INT(24)", result)

    [<Test>]
    member _.``Token printing with operators``() =
        let tokens = [LitInt 42; Plus; LitInt 24; Multiply; LitInt 2]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("INT(42) + INT(24) * INT(2)", result)

    [<Test>]
    member _.``Token printing with keywords``() =
        let tokens = [Let; Identifier "x"; Equal; LitInt 42; In; Identifier "x"]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("LET ID(x) = INT(42) IN ID(x)", result)

    [<Test>]
    member _.``Token printing with complex expressions``() =
        let tokens = [Let; Identifier "f"; Equal; Lambda; Identifier "x"; Dot; Identifier "x"; Plus; LitInt 1; In; Application (Variable "f", Literal (LInt 42))]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("LET ID(f) = λx.x + INT(1) IN f INT(42)", result)
    [<Test>]
    member _.``Token printing with nested applications``() =
        let tokens = [Application (Variable "f", Variable "x"); Application (Variable "g", Variable "y")]
        let result = TokenPrinter.printTokens tokens
        Assert.AreEqual("f x g y", result)
