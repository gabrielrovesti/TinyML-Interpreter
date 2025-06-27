/// Parser for TinyML
/// Converts tokens into Abstract Syntax Tree using recursive descent parsing
namespace TinyML

open TinyML

/// Parser state and error handling
type ParserState = {
    Tokens: Token list
    Position: int
}

/// Parser exceptions
exception ParseException of string * ParserState

/// Parser implementation using recursive descent
module Parser =
    
    /// Create initial parser state
    let create (tokens: Token list) : ParserState =
        { Tokens = tokens; Position = 0 }

    /// Get current token
    let currentToken (state: ParserState) : Token =
        if state.Position < List.length state.Tokens then
            state.Tokens.[state.Position]
        else
            EOF

    /// Peek at next token
    let peekToken (state: ParserState) : Token =
        let nextPos = state.Position + 1
        if nextPos < List.length state.Tokens then
            state.Tokens.[nextPos]
        else
            EOF

    /// Advance to next token
    let advance (state: ParserState) : ParserState =
        { state with Position = state.Position + 1 }

    /// Check if current token matches expected token
    let check (expected: Token) (state: ParserState) : bool =
        currentToken state = expected

    /// Consume expected token or raise error
    let consume (expected: Token) (state: ParserState) : ParserState =
        if check expected state then
            advance state
        else
            let current = currentToken state
            raise (ParseException (sprintf "Expected %s but found %s" 
                (TokenPrinter.printToken expected) 
                (TokenPrinter.printToken current), state))

    /// Skip newlines
    let rec skipNewlines (state: ParserState) : ParserState =
        match currentToken state with
        | Newline -> skipNewlines (advance state)
        | _ -> state

    /// Parse literal
    let parseLiteral (state: ParserState) : Expr * ParserState =
        match currentToken state with
        | LitInt i -> (Literal (LInt i), advance state)
        | LitFloat f -> (Literal (LFloat f), advance state)
        | LitBool b -> (Literal (LBool b), advance state)
        | LitString s -> (Literal (LString s), advance state)
        | LitChar c -> (Literal (LChar c), advance state)
        | LitUnit -> (Literal LUnit, advance state)
        | _ -> 
            raise (ParseException ("Expected literal", state))

    /// Parse identifier
    let parseIdentifier (state: ParserState) : string * ParserState =
        match currentToken state with
        | Identifier id -> (id, advance state)
        | _ -> 
            raise (ParseException ("Expected identifier", state))

    /// Forward declarations for mutual recursion
    let rec parseExpression (state: ParserState) : Expr * ParserState = 
        parseOrExpression state

    /// Parse primary expressions (literals, variables, parenthesized expressions, tuples)
    and parsePrimary (state: ParserState) : Expr * ParserState =
        let state = skipNewlines state
        match currentToken state with
        | LitInt _ | LitFloat _ | LitBool _ | LitString _ | LitChar _ | LitUnit ->
            parseLiteral state
        
        | Identifier id ->
            (Variable id, advance state)
        
        | LeftParen ->
            let state = advance state // consume '('
            let state = skipNewlines state
            
            // Check for unit literal ()
            if check RightParen state then
                (Literal LUnit, advance state)
            else
                let (expr, state) = parseExpression state
                let state = skipNewlines state
                
                // Check if this is a tuple
                if check Comma state then
                    let (exprs, state) = parseTupleRest [expr] state
                    let state = consume RightParen state
                    (Tuple exprs, state)
                else
                    let state = consume RightParen state
                    (expr, state)
        
        | Lambda ->
            parseLambda state
        
        | Let ->
            parseLetExpression state
        
        | If ->
            parseIfExpression state
        
        | _ ->
            raise (ParseException (sprintf "Unexpected token: %s" 
                (TokenPrinter.printToken (currentToken state)), state))

    /// Parse tuple rest (after first element and comma)
    and parseTupleRest (acc: Expr list) (state: ParserState) : Expr list * ParserState =
        let state = consume Comma state
        let state = skipNewlines state
        let (expr, state) = parseExpression state
        let state = skipNewlines state
        
        let acc = List.rev (expr :: (List.rev acc))
        
        if check Comma state then
            parseTupleRest acc state
        else
            (acc, state)

    /// Parse lambda expression: λx.e
    and parseLambda (state: ParserState) : Expr * ParserState =
        let state = consume Lambda state
        let (param, state) = parseIdentifier state
        let state = consume Dot state
        let (body, state) = parseExpression state
        (Lambda (param, body), state)

    /// Parse let expression: let x = e1 in e2 | let rec f = λx.e1 in e2
    and parseLetExpression (state: ParserState) : Expr * ParserState =
        let state = consume Let state
        let state = skipNewlines state
        
        if check Rec state then
            // Recursive let
            let state = advance state // consume 'rec'
            let (funcName, state) = parseIdentifier state
            let state = consume Equal state
            let state = skipNewlines state
            
            // Expect lambda
            if not (check Lambda state) then
                raise (ParseException ("Expected lambda in let rec", state))
            
            let state = consume Lambda state
            let (param, state) = parseIdentifier state
            let state = consume Dot state
            let (body, state) = parseExpression state
            let state = skipNewlines state
            let state = consume In state
            let state = skipNewlines state
            let (inExpr, state) = parseExpression state
            
            (LetRec (funcName, param, body, inExpr), state)
        else
            // Regular let
            let (varName, state) = parseIdentifier state
            let state = consume Equal state
            let state = skipNewlines state
            let (bindExpr, state) = parseExpression state
            let state = skipNewlines state
            let state = consume In state
            let state = skipNewlines state
            let (inExpr, state) = parseExpression state
            
            (Let (varName, bindExpr, inExpr), state)

    /// Parse if expression: if e1 then e2 else e3
    and parseIfExpression (state: ParserState) : Expr * ParserState =
        let state = consume If state
        let state = skipNewlines state
        let (condExpr, state) = parseExpression state
        let state = skipNewlines state
        let state = consume Then state
        let state = skipNewlines state
        let (thenExpr, state) = parseExpression state
        let state = skipNewlines state
        let state = consume Else state
        let state = skipNewlines state
        let (elseExpr, state) = parseExpression state
        
        (IfThenElse (condExpr, thenExpr, elseExpr), state)

    /// Parse function application (left-associative)
    and parseApplication (state: ParserState) : Expr * ParserState =
        let (expr, state) = parsePrimary state
        parseApplicationRest expr state

    and parseApplicationRest (func: Expr) (state: ParserState) : Expr * ParserState =
        let state = skipNewlines state
        match currentToken state with
        | LitInt _ | LitFloat _ | LitBool _ | LitString _ | LitChar _ | LitUnit
        | Identifier _ | LeftParen | Lambda | Let | If ->
            let (arg, state) = parsePrimary state
            let app = Application (func, arg)
            parseApplicationRest app state
        | _ ->
            (func, state)

    /// Parse unary expressions (not)
    and parseUnary (state: ParserState) : Expr * ParserState =
        match currentToken state with
        | Not ->
            let state = advance state
            let (expr, state) = parseUnary state
            // Convert 'not e' to function application
            (Application (Variable "not", expr), state)
        | Minus ->
            let state = advance state
            let (expr, state) = parseUnary state
            // Convert '-e' to '0 - e'
            (BinOp (Sub, Literal (LInt 0), expr), state)
        | _ ->
            parseApplication state

    /// Parse binary expressions with precedence climbing
    and parseBinaryExpression (minPrec: int) (state: ParserState) : Expr * ParserState =
        let (left, state) = parseUnary state
        parseBinaryRest left minPrec state

    and parseBinaryRest (left: Expr) (minPrec: int) (state: ParserState) : Expr * ParserState =
        let state = skipNewlines state
        let token = currentToken state
        
        if LexerUtils.isOperator token then
            let prec = LexerUtils.getOperatorPrecedence token
            if prec >= minPrec then
                let op = tokenToBinOp token
                let state = advance state
                let nextMinPrec = if LexerUtils.isLeftAssociative token then prec + 1 else prec
                let (right, state) = parseBinaryExpression nextMinPrec state
                let expr = BinOp (op, left, right)
                parseBinaryRest expr minPrec state
            else
                (left, state)
        else
            (left, state)

    /// Parse OR expression
    and parseOrExpression (state: ParserState) : Expr * ParserState =
        parseBinaryExpression 1 state

    /// Convert token to binary operator
    and tokenToBinOp = function
        | Plus -> Add | Minus -> Sub | Multiply -> Mul | Divide -> Div | Modulo -> Mod
        | Equal -> Eq | NotEqual -> Ne 
        | LessThan -> Lt | LessEqual -> Le | GreaterThan -> Gt | GreaterEqual -> Ge
        | LogicalAnd -> And | LogicalOr -> Or
        | _ -> failwith "Not a binary operator token"

    /// Parse sequence of expressions separated by semicolons
    let rec parseSequence (state: ParserState) : Expr * ParserState =
        let (expr, state) = parseExpression state
        let state = skipNewlines state
        
        if check Semicolon state then
            let state = advance state // consume ';'
            let state = skipNewlines state
            let (nextExpr, state) = parseSequence state
            (Sequence (expr, nextExpr), state)
        else
            (expr, state)

    /// Parse top-level expression
    let parseTopLevel (state: ParserState) : Expr * ParserState =
        let state = skipNewlines state
        parseSequence state

    /// Parse complete program
    let parse (tokens: Token list) : Expr =
        let state = create tokens
        let (expr, state) = parseTopLevel state
        let state = skipNewlines state
        
        if currentToken state <> EOF then
            raise (ParseException (sprintf "Unexpected token after expression: %s" 
                (TokenPrinter.printToken (currentToken state)), state))
        
        expr

    /// Parse expression from string
    let parseString (input: string) : Expr =
        try
            let tokens = Lexer.tokenize input
            parse tokens
        with
        | LexerException (msg, pos) ->
            failwith (sprintf "Lexer error at line %d, column %d: %s" pos.Line pos.Column msg)
        | ParseException (msg, state) ->
            failwith (sprintf "Parse error at position %d: %s" state.Position msg)

/// Parser utilities and helpers
module ParserUtils =
    
    /// Check if expression is a lambda
    let isLambda = function
        | Lambda _ -> true
        | _ -> false

    /// Check if expression is an application
    let isApplication = function
        | Application _ -> true
        | _ -> false

    /// Check if expression is a literal
    let isLiteral = function
        | Literal _ -> true
        | _ -> false

    /// Extract lambda parameters and body (for curried functions)
    let rec extractLambdaParams = function
        | Lambda (param, body) ->
            let (params, finalBody) = extractLambdaParams body
            (param :: params, finalBody)
        | expr -> ([], expr)

    /// Reconstruct curried lambda from parameters and body
    let rec makeCurriedLambda (params: string list) (body: Expr) : Expr =
        match params with
        | [] -> body
        | param :: rest ->
            Lambda (param, makeCurriedLambda rest body)

    /// Extract application spine (function and arguments)
    let rec extractApplicationSpine (expr: Expr) : Expr * Expr list =
        match expr with
        | Application (func, arg) ->
            let (baseFunc, args) = extractApplicationSpine func
            (baseFunc, args @ [arg])
        | _ -> (expr, [])

    /// Reconstruct application from function and arguments
    let rec makeApplication (func: Expr) (args: Expr list) : Expr =
        match args with
        | [] -> func
        | arg :: rest ->
            makeApplication (Application (func, arg)) rest

    /// Pretty print AST with proper parentheses
    let rec prettyPrint (expr: Expr) : string =
        match expr with
        | Literal lit -> AstPrinter.printValue (VLiteral lit)
        | Variable x -> x
        | Lambda (param, body) ->
            let (params, finalBody) = extractLambdaParams expr
            let paramStr = String.concat " " params
            sprintf "λ%s.%s" paramStr (prettyPrint finalBody)
        | Application (func, arg) ->
            let (baseFunc, args) = extractApplicationSpine expr
            let funcStr = 
                match baseFunc with
                | Lambda _ -> sprintf "(%s)" (prettyPrint baseFunc)
                | _ -> prettyPrint baseFunc
            let argStrs = args |> List.map (fun arg ->
                match arg with
                | Application _ | Lambda _ | Let _ | LetRec _ | IfThenElse _ ->
                    sprintf "(%s)" (prettyPrint arg)
                | _ -> prettyPrint arg)
            sprintf "%s %s" funcStr (String.concat " " argStrs)
        | Let (var, expr, body) ->
            sprintf "let %s = %s in %s" var (prettyPrint expr) (prettyPrint body)
        | LetRec (func, param, expr, body) ->
            sprintf "let rec %s = λ%s.%s in %s" func param (prettyPrint expr) (prettyPrint body)
        | IfThenElse (cond, thenExpr, elseExpr) ->
            sprintf "if %s then %s else %s" 
                (prettyPrint cond) (prettyPrint thenExpr) (prettyPrint elseExpr)
        | Tuple exprs ->
            exprs 
            |> List.map prettyPrint 
            |> String.concat ", "
            |> sprintf "(%s)"
        | BinOp (op, left, right) ->
            let opStr = AstPrinter.printBinOp op
            let leftStr = 
                match left with
                | BinOp (leftOp, _, _) when LexerUtils.getOperatorPrecedence (binOpToToken leftOp) < 
                                           LexerUtils.getOperatorPrecedence (binOpToToken op) ->
                    sprintf "(%s)" (prettyPrint left)
                | _ -> prettyPrint left
            let rightStr = 
                match right with
                | BinOp (rightOp, _, _) when LexerUtils.getOperatorPrecedence (binOpToToken rightOp) <= 
                                             LexerUtils.getOperatorPrecedence (binOpToToken op) ->
                    sprintf "(%s)" (prettyPrint right)
                | _ -> prettyPrint right
            sprintf "%s %s %s" leftStr opStr rightStr
        | Sequence (expr1, expr2) ->
            sprintf "%s; %s" (prettyPrint expr1) (prettyPrint expr2)

    and binOpToToken = function
        | Add -> Plus | Sub -> Minus | Mul -> Multiply | Div -> Divide | Mod -> Modulo
        | Eq -> Equal | Ne -> NotEqual | Lt -> LessThan | Le -> LessEqual 
        | Gt -> GreaterThan | Ge -> GreaterEqual | And -> LogicalAnd | Or -> LogicalOr

    /// Validate AST structure
    let rec validateAst (expr: Expr) : bool =
        match expr with
        | Literal _ | Variable _ -> true
        | Lambda (_, body) -> validateAst body
        | Application (func, arg) -> validateAst func && validateAst arg
        | Let (_, bindExpr, body) -> validateAst bindExpr && validateAst body
        | LetRec (_, _, bindExpr, body) -> validateAst bindExpr && validateAst body
        | IfThenElse (cond, thenExpr, elseExpr) -> 
            validateAst cond && validateAst thenExpr && validateAst elseExpr
        | Tuple exprs -> List.forall validateAst exprs
        | BinOp (_, left, right) -> validateAst left && validateAst right
        | Sequence (expr1, expr2) -> validateAst expr1 && validateAst expr2