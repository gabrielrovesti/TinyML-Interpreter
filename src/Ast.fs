/// Abstract Syntax Tree definitions for TinyML
/// Following the formal syntax from Advanced Notes on ML v1.7
namespace TinyML

/// Literals in TinyML
type Literal =
    | LInt of int          // integers
    | LFloat of float      // floating point numbers
    | LBool of bool        // booleans
    | LString of string    // strings
    | LChar of char        // characters
    | LUnit                // unit value ()

/// Binary operators
type BinOp =
    | Add | Sub | Mul | Div | Mod    // arithmetic
    | Eq | Ne | Lt | Le | Gt | Ge    // comparison
    | And | Or                       // logical

/// Expressions following the formal grammar e ::= ...
type Expr =
    | Literal of Literal                           // L
    | Variable of string                           // x
    | Lambda of string * Expr                      // λx.e
    | Application of Expr * Expr                   // e e
    | Let of string * Expr * Expr                  // let x = e in e
    | LetRec of string * string * Expr * Expr      // let rec f = λx.e in e
    | IfThenElse of Expr * Expr * Expr            // if e then e else e
    | Tuple of Expr list                          // (e1, ..., en)
    | BinOp of BinOp * Expr * Expr                // e op e
    | Sequence of Expr * Expr                     // e; e (extension)

/// Type constructors
type TypeName =
    | TInt | TFloat | TBool | TString | TChar | TUnit

/// Types following the formal grammar τ ::= ...
type Type =
    | TyCon of TypeName                    // c (type constructors)
    | TyVar of int                         // α (type variables as integers)
    | TyArrow of Type * Type               // τ → τ (function types)
    | TyTuple of Type list                 // τ * ... * τ (tuple types)

/// Type schemes σ ::= ∀α.τ
type TypeScheme = 
    | Forall of int list * Type            // ∀α₁...αₙ.τ

/// Values following the formal grammar v ::= ...
type Value =
    | VLiteral of Literal                              // L
    | VClosure of string * Expr * Environment         // ⟨λx.e, Δ⟩
    | VRecClosure of string * string * Expr * Environment  // ⟨λx.e, f, Δ⟩
    | VTuple of Value list                             // (v1, ..., vn)

/// Evaluation environment Δ ::= ∅ | Δ,(x ↦ v)
and Environment = Map<string, Value>

/// Typing environment Γ ::= ∅ | Γ,(x : σ)
type TypeEnvironment = Map<string, TypeScheme>

/// Substitution θ ::= ∅ | θ,[α ↦ τ]
type Substitution = Map<int, Type>

/// Pretty printing utilities
module AstPrinter =
    
    let rec printType ty =
        match ty with
        | TyCon TInt -> "int"
        | TyCon TFloat -> "float"
        | TyCon TBool -> "bool"
        | TyCon TString -> "string"
        | TyCon TChar -> "char"
        | TyCon TUnit -> "unit"
        | TyVar n -> sprintf "'%c" (char (int 'a' + n % 26))
        | TyArrow (t1, t2) -> sprintf "(%s -> %s)" (printType t1) (printType t2)
        | TyTuple types -> 
            types 
            |> List.map printType 
            |> String.concat " * "
            |> sprintf "(%s)"

    let printTypeScheme (Forall (vars, ty)) =
        if List.isEmpty vars then
            printType ty
        else
            let varStr = vars |> List.map (fun v -> sprintf "'%c" (char (int 'a' + v % 26))) |> String.concat " "
            sprintf "∀%s.%s" varStr (printType ty)

    let rec printValue value =
        match value with
        | VLiteral (LInt i) -> string i
        | VLiteral (LFloat f) -> string f
        | VLiteral (LBool b) -> string b
        | VLiteral (LString s) -> sprintf "\"%s\"" s
        | VLiteral (LChar c) -> sprintf "'%c'" c
        | VLiteral LUnit -> "()"
        | VClosure (param, _, _) -> sprintf "<fun:%s>" param
        | VRecClosure (param, name, _, _) -> sprintf "<rec_fun:%s:%s>" name param
        | VTuple values -> 
            values 
            |> List.map printValue 
            |> String.concat ", "
            |> sprintf "(%s)"

    let printBinOp op =
        match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
        | Eq -> "=" | Ne -> "<>" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||"

    let rec printExpr expr =
        match expr with
        | Literal lit -> 
            match lit with
            | LInt i -> string i
            | LFloat f -> string f
            | LBool b -> string b
            | LString s -> sprintf "\"%s\"" s
            | LChar c -> sprintf "'%c'" c
            | LUnit -> "()"
        | Variable x -> x
        | Lambda (x, e) -> sprintf "(λ%s.%s)" x (printExpr e)
        | Application (e1, e2) -> sprintf "(%s %s)" (printExpr e1) (printExpr e2)
        | Let (x, e1, e2) -> sprintf "(let %s = %s in %s)" x (printExpr e1) (printExpr e2)
        | LetRec (f, x, e1, e2) -> sprintf "(let rec %s = λ%s.%s in %s)" f x (printExpr e1) (printExpr e2)
        | IfThenElse (e1, e2, e3) -> sprintf "(if %s then %s else %s)" (printExpr e1) (printExpr e2) (printExpr e3)
        | Tuple exprs -> 
            exprs 
            |> List.map printExpr 
            |> String.concat ", "
            |> sprintf "(%s)"
        | BinOp (op, e1, e2) -> sprintf "(%s %s %s)" (printExpr e1) (printBinOp op) (printExpr e2)
        | Sequence (e1, e2) -> sprintf "%s; %s" (printExpr e1) (printExpr e2)

/// Utility functions for AST manipulation
module AstUtils =
    
    /// Generate fresh type variable
    let mutable typeVarCounter = 0
    let freshTypeVar () =
        let var = typeVarCounter
        typeVarCounter <- typeVarCounter + 1
        TyVar var

    /// Reset type variable counter (useful for testing)
    let resetTypeVars () =
        typeVarCounter <- 0

    /// Get free variables in an expression
    let rec freeVars expr =
        match expr with
        | Literal _ -> Set.empty
        | Variable x -> Set.singleton x
        | Lambda (x, e) -> Set.remove x (freeVars e)
        | Application (e1, e2) -> Set.union (freeVars e1) (freeVars e2)
        | Let (x, e1, e2) -> Set.union (freeVars e1) (Set.remove x (freeVars e2))
        | LetRec (f, x, e1, e2) -> 
            let fvE1 = Set.remove f (Set.remove x (freeVars e1))
            let fvE2 = Set.remove f (freeVars e2)
            Set.union fvE1 fvE2
        | IfThenElse (e1, e2, e3) -> 
            [freeVars e1; freeVars e2; freeVars e3] |> Set.unionMany
        | Tuple exprs -> 
            exprs |> List.map freeVars |> Set.unionMany
        | BinOp (_, e1, e2) -> Set.union (freeVars e1) (freeVars e2)
        | Sequence (e1, e2) -> Set.union (freeVars e1) (freeVars e2)

    /// Get free type variables in a type
    let rec freeTypeVars ty =
        match ty with
        | TyCon _ -> Set.empty
        | TyVar n -> Set.singleton n
        | TyArrow (t1, t2) -> Set.union (freeTypeVars t1) (freeTypeVars t2)
        | TyTuple types -> types |> List.map freeTypeVars |> Set.unionMany

    /// Get free type variables in a type scheme
    let freeTypeVarsScheme (Forall (bound, ty)) =
        Set.difference (freeTypeVars ty) (Set.ofList bound)

    /// Get free type variables in a type environment
    let freeTypeVarsEnv (env: TypeEnvironment) =
        env 
        |> Map.values 
        |> Seq.map freeTypeVarsScheme 
        |> Set.unionMany