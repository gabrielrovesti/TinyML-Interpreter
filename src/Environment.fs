/// Environment management for TinyML
/// Handles both typing environments Γ and evaluation environments Δ
namespace TinyML

open TinyML

/// Evaluation environment operations
module EvalEnv =
    
    /// Empty evaluation environment
    let empty : Environment = Map.empty

    /// Lookup variable in environment
    let lookup (env: Environment) (var: string) : Value option =
        Map.tryFind var env

    /// Extend environment with new binding
    let extend (env: Environment) (var: string) (value: Value) : Environment =
        Map.add var env value

    /// Extend environment with multiple bindings
    let extendMultiple (env: Environment) (bindings: (string * Value) list) : Environment =
        List.fold (fun acc (var, value) -> extend acc var value) env bindings

    /// Check if variable is bound in environment
    let contains (env: Environment) (var: string) : bool =
        Map.containsKey var env

    /// Get all bound variables
    let domain (env: Environment) : string list =
        Map.keys env |> List.ofSeq

    /// Get all values
    let codomain (env: Environment) : Value list =
        Map.values env |> List.ofSeq

    /// Remove binding from environment
    let remove (env: Environment) (var: string) : Environment =
        Map.remove var env

    /// Convert to list of bindings
    let toList (env: Environment) : (string * Value) list =
        Map.toList env

    /// Create from list of bindings
    let fromList (bindings: (string * Value) list) : Environment =
        Map.ofList bindings

    /// Pretty print environment
    let print (env: Environment) : string =
        env
        |> Map.toList
        |> List.map (fun (var, value) -> sprintf "%s ↦ %s" var (AstPrinter.printValue value))
        |> String.concat ", "
        |> sprintf "{%s}"

/// Type environment operations
module TypeEnv =
    
    /// Empty type environment
    let empty : TypeEnvironment = Map.empty

    /// Lookup variable in type environment
    let lookup (env: TypeEnvironment) (var: string) : TypeScheme option =
        Map.tryFind var env

    /// Extend environment with new type binding
    let extend (env: TypeEnvironment) (var: string) (scheme: TypeScheme) : TypeEnvironment =
        Map.add var scheme env

    /// Extend environment with type (automatically generalized)
    let extendWithType (env: TypeEnvironment) (var: string) (ty: Type) : TypeEnvironment =
        let scheme = TypeOps.generalize env ty
        extend env var scheme

    /// Extend environment with multiple bindings
    let extendMultiple (env: TypeEnvironment) (bindings: (string * TypeScheme) list) : TypeEnvironment =
        List.fold (fun acc (var, scheme) -> extend acc var scheme) env bindings

    /// Check if variable is bound in environment
    let contains (env: TypeEnvironment) (var: string) : bool =
        Map.containsKey var env

    /// Get all bound variables
    let domain (env: TypeEnvironment) : string list =
        Map.keys env |> List.ofSeq

    /// Get all type schemes
    let codomain (env: TypeEnvironment) : TypeScheme list =
        Map.values env |> List.ofSeq

    /// Remove binding from environment
    let remove (env: TypeEnvironment) (var: string) : TypeEnvironment =
        Map.remove var env

    /// Convert to list of bindings
    let toList (env: TypeEnvironment) : (string * TypeScheme) list =
        Map.toList env

    /// Create from list of bindings
    let fromList (bindings: (string * TypeScheme) list) : TypeEnvironment =
        Map.ofList bindings

    /// Apply substitution to entire environment
    let applySubst (subst: Substitution) (env: TypeEnvironment) : TypeEnvironment =
        TypeOps.applySubstEnv subst env

    /// Get free type variables in environment
    let freeTypeVars (env: TypeEnvironment) : Set<int> =
        AstUtils.freeTypeVarsEnv env

    /// Pretty print type environment
    let print (env: TypeEnvironment) : string =
        env
        |> Map.toList
        |> List.map (fun (var, scheme) -> sprintf "%s : %s" var (AstPrinter.printTypeScheme scheme))
        |> String.concat ", "
        |> sprintf "{%s}"

/// Built-in functions and standard library
module Builtins =
    
    /// Built-in type environment with standard functions
    let builtinTypes : TypeEnvironment =
        let intToInt = TypeOps.generalize TypeEnv.empty (TyArrow (TyCon TInt, TyCon TInt))
        let intToIntToInt = TypeOps.generalize TypeEnv.empty 
            (TyArrow (TyCon TInt, TyArrow (TyCon TInt, TyCon TInt)))
        let intToIntToBool = TypeOps.generalize TypeEnv.empty 
            (TyArrow (TyCon TInt, TyArrow (TyCon TInt, TyCon TBool)))
        let boolToBoolToBool = TypeOps.generalize TypeEnv.empty 
            (TyArrow (TyCon TBool, TyArrow (TyCon TBool, TyCon TBool)))
        
        // Generic list operations (simplified)
        let alpha = AstUtils.freshTypeVar()
        let listType = TyTuple [alpha] // Simplified list as tuple
        let listToInt = TypeOps.generalize TypeEnv.empty (TyArrow (listType, TyCon TInt))
        let listToBool = TypeOps.generalize TypeEnv.empty (TyArrow (listType, TyCon TBool))
        
        Map.ofList [
            // Arithmetic
            ("abs", intToInt)
            ("neg", intToInt)
            ("succ", intToInt)
            ("pred", intToInt)
            
            // Comparison (already handled by BinOp)
            
            // Boolean
            ("not", TypeOps.generalize TypeEnv.empty (TyArrow (TyCon TBool, TyCon TBool)))
            
            // List operations (simplified)
            ("length", listToInt)
            ("null", listToBool)
            
            // I/O (simplified)
            ("print", TypeOps.generalize TypeEnv.empty (TyArrow (TyCon TString, TyCon TUnit)))
            ("print_int", TypeOps.generalize TypeEnv.empty (TyArrow (TyCon TInt, TyCon TUnit)))
            ("print_bool", TypeOps.generalize TypeEnv.empty (TyArrow (TyCon TBool, TyCon TUnit)))
        ]

    /// Built-in evaluation environment with standard functions
    let builtinValues : Environment =
        let rec abs_impl = function
            | VLiteral (LInt n) -> VLiteral (LInt (abs n))
            | _ -> failwith "abs: expected integer"
        
        let rec neg_impl = function
            | VLiteral (LInt n) -> VLiteral (LInt (-n))
            | _ -> failwith "neg: expected integer"
        
        let rec not_impl = function
            | VLiteral (LBool b) -> VLiteral (LBool (not b))
            | _ -> failwith "not: expected boolean"
        
        let rec print_impl = function
            | VLiteral (LString s) -> 
                printf "%s" s
                VLiteral LUnit
            | _ -> failwith "print: expected string"
        
        let rec print_int_impl = function
            | VLiteral (LInt i) -> 
                printf "%d" i
                VLiteral LUnit
            | _ -> failwith "print_int: expected integer"
        
        let rec print_bool_impl = function
            | VLiteral (LBool b) -> 
                printf "%b" b
                VLiteral LUnit
            | _ -> failwith "print_bool: expected boolean"

        // Create closures for built-in functions
        // Note: these are simplified implementations
        Map.ofList [
            ("abs", VClosure ("x", Variable "x", Map.empty)) // Simplified
            ("neg", VClosure ("x", Variable "x", Map.empty)) // Simplified
            ("not", VClosure ("x", Variable "x", Map.empty)) // Simplified
            ("print", VClosure ("x", Variable "x", Map.empty)) // Simplified
            ("print_int", VClosure ("x", Variable "x", Map.empty)) // Simplified
            ("print_bool", VClosure ("x", Variable "x", Map.empty)) // Simplified
        ]

    /// Create initial environment with builtins
    let initialTypeEnv () : TypeEnvironment = builtinTypes
    let initialEvalEnv () : Environment = builtinValues

/// Environment utilities and helpers
module EnvUtils =
    
    /// Merge two environments (right environment takes precedence)
    let mergeEvalEnv (env1: Environment) (env2: Environment) : Environment =
        Map.fold Map.add env1 env2

    let mergeTypeEnv (env1: TypeEnvironment) (env2: TypeEnvironment) : TypeEnvironment =
        Map.fold Map.add env1 env2

    /// Filter environment by predicate on variables
    let filterEvalEnv (predicate: string -> bool) (env: Environment) : Environment =
        env |> Map.filter (fun var _ -> predicate var)

    let filterTypeEnv (predicate: string -> bool) (env: TypeEnvironment) : TypeEnvironment =
        env |> Map.filter (fun var _ -> predicate var)

    /// Get variables that are in first environment but not in second
    let diffEvalEnv (env1: Environment) (env2: Environment) : Environment =
        env1 |> Map.filter (fun var _ -> not (Map.containsKey var env2))

    let diffTypeEnv (env1: TypeEnvironment) (env2: TypeEnvironment) : TypeEnvironment =
        env1 |> Map.filter (fun var _ -> not (Map.containsKey var env2))

    /// Check if all variables in the list are bound in the environment
    let allBoundEval (vars: string list) (env: Environment) : bool =
        List.forall (fun var -> Map.containsKey var env) vars

    let allBoundType (vars: string list) (env: TypeEnvironment) : bool =
        List.forall (fun var -> Map.containsKey var env) vars

    /// Get unbound variables from a list
    let getUnboundEval (vars: string list) (env: Environment) : string list =
        vars |> List.filter (fun var -> not (Map.containsKey var env))

    let getUnboundType (vars: string list) (env: TypeEnvironment) : string list =
        vars |> List.filter (fun var -> not (Map.containsKey var env))

    /// Create a scope: execute function with extended environment, then restore
    let withScopeEval (env: Environment) (bindings: (string * Value) list) (f: Environment -> 'a) : 'a =
        let extendedEnv = EvalEnv.extendMultiple env bindings
        f extendedEnv

    let withScopeType (env: TypeEnvironment) (bindings: (string * TypeScheme) list) (f: TypeEnvironment -> 'a) : 'a =
        let extendedEnv = TypeEnv.extendMultiple env bindings
        f extendedEnv

    /// Validate environment consistency (for debugging)
    let validateEnvConsistency (typeEnv: TypeEnvironment) (evalEnv: Environment) : bool =
        let typeVars = TypeEnv.domain typeEnv |> Set.ofList
        let evalVars = EvalEnv.domain evalEnv |> Set.ofList
        // Check that all variables in eval env have types (but not vice versa due to built-ins)
        Set.isSubset evalVars typeVars