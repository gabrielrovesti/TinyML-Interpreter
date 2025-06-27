/// Lexical analyzer for TinyML
/// Converts source code into tokens for parsing
namespace TinyML

/// Token types for TinyML
type Token =
    // Literals
    | LitInt of int
    | LitFloat of float
    | LitBool of bool
    | LitString of string
    | LitChar of char
    | LitUnit
    
    // Identifiers and keywords
    | Identifier of string
    | Lambda | Let | Rec | In | If | Then | Else | And | Or
    
    // Operators and punctuation
    | Plus | Minus | Multiply | Divide | Modulo
    | Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual
    | LogicalAnd | LogicalOr | Not
    | Arrow | Assign
    
    // Delimiters
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftBrace | RightBrace
    | Comma | Semicolon | Dot | Colon
    
    // Special
    | EOF
    | Newline

/// Lexer state and position tracking
type Position = { Line: int; Column: int; Index: int }

type LexerState = {
    Input: string
    Position: Position
    Current: char option
}

/// Lexer exceptions
exception LexerException of string * Position

/// Lexical analyzer implementation
module Lexer =
    
    /// Create initial lexer state
    let create (input: string) : LexerState =
        let pos = { Line = 1; Column = 1; Index = 0 }
        let current = if String.length input > 0 then Some input.[0] else None
        { Input = input; Position = pos; Current = current }

    /// Advance to next character
    let advance (state: LexerState) : LexerState =
        match state.Current with
        | None -> state
        | Some ch ->
            let newIndex = state.Position.Index + 1
            let newPos = 
                if ch = '\n' then
                    { Line = state.Position.Line + 1; Column = 1; Index = newIndex }
                else
                    { state.Position with Column = state.Position.Column + 1; Index = newIndex }
            
            let newCurrent = 
                if newIndex < String.length state.Input then
                    Some state.Input.[newIndex]
                else
                    None
            
            { state with Position = newPos; Current = newCurrent }

    /// Peek at next character without advancing
    let peek (state: LexerState) : char option =
        let nextIndex = state.Position.Index + 1
        if nextIndex < String.length state.Input then
            Some state.Input.[nextIndex]
        else
            None

    /// Skip whitespace (except newlines which are significant)
    let rec skipWhitespace (state: LexerState) : LexerState =
        match state.Current with
        | Some (' ' | '\t' | '\r') -> skipWhitespace (advance state)
        | _ -> state

    /// Skip line comments (// to end of line)
    let rec skipLineComment (state: LexerState) : LexerState =
        match state.Current with
        | None | Some '\n' -> state
        | _ -> skipLineComment (advance state)

    /// Skip block comments (* ... *)
    let rec skipBlockComment (state: LexerState) (depth: int) : LexerState =
        match state.Current, peek state with
        | None, _ -> 
            raise (LexerException ("Unterminated block comment", state.Position))
        | Some '*', Some ')' ->
            let newDepth = depth - 1
            let state' = advance (advance state)
            if newDepth = 0 then state' else skipBlockComment state' newDepth
        | Some '(', Some '*' ->
            let newDepth = depth + 1
            let state' = advance (advance state)
            skipBlockComment state' newDepth
        | _ ->
            skipBlockComment (advance state) depth

    /// Read string literal
    let readString (state: LexerState) : string * LexerState =
        let rec loop acc state =
            match state.Current with
            | None -> 
                raise (LexerException ("Unterminated string literal", state.Position))
            | Some '"' -> 
                (acc, advance state)
            | Some '\\' ->
                match peek state with
                | Some 'n' -> loop (acc + "\n") (advance (advance state))
                | Some 't' -> loop (acc + "\t") (advance (advance state))
                | Some 'r' -> loop (acc + "\r") (advance (advance state))
                | Some '\\' -> loop (acc + "\\") (advance (advance state))
                | Some '"' -> loop (acc + "\"") (advance (advance state))
                | Some '\'' -> loop (acc + "'") (advance (advance state))
                | Some c -> loop (acc + string c) (advance (advance state))
                | None -> raise (LexerException ("Unterminated string literal", state.Position))
            | Some c ->
                loop (acc + string c) (advance state)
        loop "" (advance state) // Skip opening quote

    /// Read character literal
    let readChar (state: LexerState) : char * LexerState =
        let state = advance state // Skip opening quote
        match state.Current with
        | None -> 
            raise (LexerException ("Unterminated character literal", state.Position))
        | Some '\\' ->
            match peek state with
            | Some 'n' -> ('\n', advance (advance (advance state))) // Skip char, backslash, closing quote
            | Some 't' -> ('\t', advance (advance (advance state)))
            | Some 'r' -> ('\r', advance (advance (advance state)))
            | Some '\\' -> ('\\', advance (advance (advance state)))
            | Some '\'' -> ('\'', advance (advance (advance state)))
            | Some '"' -> ('"', advance (advance (advance state)))
            | Some c -> (c, advance (advance (advance state)))
            | None -> raise (LexerException ("Unterminated character literal", state.Position))
        | Some c ->
            let state = advance state
            match state.Current with
            | Some '\'' -> (c, advance state)
            | _ -> raise (LexerException ("Character literal must be single character", state.Position))

    /// Read number (integer or float)
    let readNumber (state: LexerState) : Token * LexerState =
        let rec loop acc state hasDecimal =
            match state.Current with
            | Some c when System.Char.IsDigit c ->
                loop (acc + string c) (advance state) hasDecimal
            | Some '.' when not hasDecimal ->
                match peek state with
                | Some c when System.Char.IsDigit c ->
                    loop (acc + ".") (advance state) true
                | _ -> 
                    // Not a decimal, stop here
                    if hasDecimal then
                        (LitFloat (System.Double.Parse acc), state)
                    else
                        (LitInt (System.Int32.Parse acc), state)
            | _ ->
                if hasDecimal then
                    (LitFloat (System.Double.Parse acc), state)
                else
                    (LitInt (System.Int32.Parse acc), state)
        
        match state.Current with
        | Some c when System.Char.IsDigit c ->
            loop (string c) (advance state) false
        | _ -> failwith "readNumber called without digit"

    /// Read identifier or keyword
    let readIdentifier (state: LexerState) : Token * LexerState =
        let rec loop acc state =
            match state.Current with
            | Some c when System.Char.IsLetterOrDigit c || c = '_' || c = '\'' ->
                loop (acc + string c) (advance state)
            | _ -> (acc, state)
        
        let (id, newState) = loop "" state
        let token = 
            match id with
            | "let" -> Let
            | "rec" -> Rec
            | "in" -> In
            | "if" -> If
            | "then" -> Then
            | "else" -> Else
            | "and" -> And
            | "or" -> Or
            | "true" -> LitBool true
            | "false" -> LitBool false
            | "not" -> Not
            | _ -> Identifier id
        
        (token, newState)

    /// Get next token
    let nextToken (state: LexerState) : Token * LexerState =
        let rec loop state =
            let state = skipWhitespace state
            match state.Current with
            | None -> (EOF, state)
            
            | Some '\n' -> (Newline, advance state)
            
            // Comments
            | Some '/' when peek state = Some '/' ->
                loop (skipLineComment (advance (advance state)))
            | Some '(' when peek state = Some '*' ->
                loop (skipBlockComment (advance (advance state)) 1)
            
            // String literals
            | Some '"' ->
                let (str, state') = readString state
                (LitString str, state')
            
            // Character literals
            | Some '\'' ->
                let (ch, state') = readChar state
                (LitChar ch, state')
            
            // Numbers
            | Some c when System.Char.IsDigit c ->
                readNumber state
            
            // Identifiers and keywords
            | Some c when System.Char.IsLetter c || c = '_' ->
                readIdentifier state
            
            // Two-character operators
            | Some '<' when peek state = Some '=' ->
                (LessEqual, advance (advance state))
            | Some '>' when peek state = Some '=' ->
                (GreaterEqual, advance (advance state))
            | Some '<' when peek state = Some '>' ->
                (NotEqual, advance (advance state))
            | Some '&' when peek state = Some '&' ->
                (LogicalAnd, advance (advance state))
            | Some '|' when peek state = Some '|' ->
                (LogicalOr, advance (advance state))
            | Some '-' when peek state = Some '>' ->
                (Arrow, advance (advance state))
            | Some ':' when peek state = Some '=' ->
                (Assign, advance (advance state))
            
            // Single-character operators and punctuation
            | Some '+' -> (Plus, advance state)
            | Some '-' -> (Minus, advance state)
            | Some '*' -> (Multiply, advance state)
            | Some '/' -> (Divide, advance state)
            | Some '%' -> (Modulo, advance state)
            | Some '=' -> (Equal, advance state)
            | Some '<' -> (LessThan, advance state)
            | Some '>' -> (GreaterThan, advance state)
            | Some '(' -> 
                // Check for unit literal ()
                if peek state = Some ')' then
                    (LitUnit, advance (advance state))
                else
                    (LeftParen, advance state)
            | Some ')' -> (RightParen, advance state)
            | Some '[' -> (LeftBracket, advance state)
            | Some ']' -> (RightBracket, advance state)
            | Some '{' -> (LeftBrace, advance state)
            | Some '}' -> (RightBrace, advance state)
            | Some ',' -> (Comma, advance state)
            | Some ';' -> (Semicolon, advance state)
            | Some '.' -> (Dot, advance state)
            | Some ':' -> (Colon, advance state)
            | Some '\\' -> (Lambda, advance state)
            
            | Some c ->
                raise (LexerException (sprintf "Unexpected character: %c" c, state.Position))
        
        loop state

    /// Tokenize entire input
    let tokenize (input: string) : Token list =
        let rec loop acc state =
            let (token, newState) = nextToken state
            match token with
            | EOF -> List.rev (token :: acc)
            | _ -> loop (token :: acc) newState
        
        loop [] (create input)

    /// Tokenize with position information
    let tokenizeWithPositions (input: string) : (Token * Position) list =
        let rec loop acc state =
            let pos = state.Position
            let (token, newState) = nextToken state
            match token with
            | EOF -> List.rev ((token, pos) :: acc)
            | _ -> loop ((token, pos) :: acc) newState
        
        loop [] (create input)

/// Pretty printing for tokens
module TokenPrinter =
    
    let printToken = function
        | LitInt i -> sprintf "INT(%d)" i
        | LitFloat f -> sprintf "FLOAT(%g)" f
        | LitBool b -> sprintf "BOOL(%b)" b
        | LitString s -> sprintf "STRING(\"%s\")" s
        | LitChar c -> sprintf "CHAR('%c')" c
        | LitUnit -> "UNIT"
        | Identifier id -> sprintf "ID(%s)" id
        | Lambda -> "LAMBDA"
        | Let -> "LET" | Rec -> "REC" | In -> "IN"
        | If -> "IF" | Then -> "THEN" | Else -> "ELSE"
        | And -> "AND" | Or -> "OR" | Not -> "NOT"
        | Plus -> "+" | Minus -> "-" | Multiply -> "*" | Divide -> "/" | Modulo -> "%"
        | Equal -> "=" | NotEqual -> "<>" 
        | LessThan -> "<" | LessEqual -> "<=" 
        | GreaterThan -> ">" | GreaterEqual -> ">="
        | LogicalAnd -> "&&" | LogicalOr -> "||"
        | Arrow -> "->" | Assign -> ":="
        | LeftParen -> "(" | RightParen -> ")"
        | LeftBracket -> "[" | RightBracket -> "]"
        | LeftBrace -> "{" | RightBrace -> "}"
        | Comma -> "," | Semicolon -> ";" | Dot -> "." | Colon -> ":"
        | EOF -> "EOF" | Newline -> "NEWLINE"

    let printTokens (tokens: Token list) : string =
        tokens
        |> List.map printToken
        |> String.concat " "

    let printTokensWithPositions (tokens: (Token * Position) list) : string =
        tokens
        |> List.map (fun (token, pos) -> 
            sprintf "%s@%d:%d" (printToken token) pos.Line pos.Column)
        |> String.concat " "

/// Lexer utilities and helpers
module LexerUtils =
    
    /// Check if token is a literal
    let isLiteral = function
        | LitInt _ | LitFloat _ | LitBool _ | LitString _ | LitChar _ | LitUnit -> true
        | _ -> false

    /// Check if token is an operator
    let isOperator = function
        | Plus | Minus | Multiply | Divide | Modulo
        | Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual
        | LogicalAnd | LogicalOr | Not -> true
        | _ -> false

    /// Check if token is a keyword
    let isKeyword = function
        | Lambda | Let | Rec | In | If | Then | Else | And | Or -> true
        | _ -> false

    /// Check if token is a delimiter
    let isDelimiter = function
        | LeftParen | RightParen | LeftBracket | RightBracket | LeftBrace | RightBrace
        | Comma | Semicolon | Dot | Colon | Arrow | Assign -> true
        | _ -> false

    /// Get operator precedence (higher number = higher precedence)
    let getOperatorPrecedence = function
        | LogicalOr -> 1
        | LogicalAnd -> 2
        | Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual -> 3
        | Plus | Minus -> 4
        | Multiply | Divide | Modulo -> 5
        | Not -> 6  // Unary
        | _ -> 0    // Not an operator

    /// Check if operator is left-associative
    let isLeftAssociative = function
        | LogicalOr | LogicalAnd 
        | Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual
        | Plus | Minus | Multiply | Divide | Modulo -> true
        | Not -> false  // Unary, not applicable
        | _ -> false

    /// Extract literal value from token
    let extractLiteral = function
        | LitInt i -> Literal (LInt i)
        | LitFloat f -> Literal (LFloat f)
        | LitBool b -> Literal (LBool b)
        | LitString s -> Literal (LString s)
        | LitChar c -> Literal (LChar c)
        | LitUnit -> Literal LUnit
        | _ -> failwith "Not a literal token"
    /// Convert token to string representation
    let tokenToString = function
        | LitInt i -> sprintf "LitInt(%d)" i
        | LitFloat f -> sprintf "LitFloat(%g)" f
        | LitBool b -> sprintf "LitBool(%b)" b
        | LitString s -> sprintf "LitString(\"%s\")" s
        | LitChar c -> sprintf "LitChar('%c')" c
        | LitUnit -> "LitUnit"
        | Identifier id -> sprintf "Identifier(%s)" id
        | Lambda -> "Lambda"
        | Let -> "Let" | Rec -> "Rec" | In -> "In"
        | If -> "If" | Then -> "Then" | Else -> "Else"
        | And -> "And" | Or -> "Or" | Not -> "Not"
        | Plus -> "+" | Minus -> "-" | Multiply -> "*" 
        | Divide -> "/" | Modulo -> "%"
        | Equal -> "=" | NotEqual -> "<>" 
        | LessThan -> "<" | LessEqual -> "<=" 
        | GreaterThan -> ">" | GreaterEqual -> ">="
        | LogicalAnd -> "LogicalAnd" 
        | LogicalOr -> "LogicalOr"
        | Arrow -> "Arrow" 
        | Assign -> "Assign"
        | LeftParen -> "(" 
        | RightParen -> ")"
        | LeftBracket -> "[" 
        | RightBracket -> "]"
        | LeftBrace -> "{" 
        | RightBrace -> "}"
        | Comma -> "," 
        | Semicolon -> ";" 
        | Dot -> "." 
        | Colon -> ":"
        | EOF -> "EOF" 
        | Newline -> "Newline"
    /// Convert token to string with position
    let tokenToStringWithPosition (token: Token, pos: Position) : string =
        sprintf "%s@%d:%d" (tokenToString token) pos.Line pos.Column
    /// Convert list of tokens to string with positions
    let tokensToStringWithPositions (tokens: (Token * Position) list) : string =
        tokens
        |> List.map tokenToStringWithPosition
        |> String.concat " "
    /// Convert list of tokens to string
    let tokensToString (tokens: Token list) : string = 
        tokens
        |> List.map tokenToString
        |> String.concat " "