/// Type checker for TinyML
/// Implementation of Hindley-Milner type inference algorithm
namespace TinyML

open TinyML

/// Type checking errors
type TypeError =
    | UnboundVariable of string
    | TypeMismatch of Type * Type * string
    | OccursCheckFailure of int * Type
    | ArityMismatch of int * int * string
    | RecursiveTypeError of string
    | InvalidRecursiveBinding of string

exception TypeException of TypeError

/// Type inference implementation
module TypeChecker =
    
    /// Type inference state containing constraints and substitutions
    type InferenceState = {
        Constraints: (Type * Type) list
        NextTypeVar: int
        Substitution: Substitution
    }

    /// Create initial inference state
    let createState () : InferenceState =
        AstUtils.resetTypeVars() // Reset global counter
        {
            Constraints = []
            NextTypeVar = 0
            Substitution = Map.empty
        }

    /// Generate fresh type variable
    let freshTypeVar (state: InferenceState) : Type * InferenceState =
        let var = TyVar state.NextTypeVar
        let newState = { state with NextTypeVar = state.NextTypeVar + 1 }
        (var, newState)

    /// Add constraint to inference state
    let addConstraint (t1: Type) (t2: Type) (state: InferenceState) : InferenceState =
        { state with Constraints = (t1, t2) :: state.Constraints }

    /// Apply current substitution to type
    let applyCurrentSubst (ty: Type) (state: InferenceState) : Type =
        TypeOps.applySubst state.Substitution ty

    /// Apply current substitution to type environment
    let applyCurrentSubstEnv (env: TypeEnvironment) (state: InferenceState) : TypeEnvironment =
        TypeOps.applySubstEnv state.Substitution env

    /// Compose substitution with current state
    let composeSubst (newSubst: Substitution) (state: InferenceState) : InferenceState =
        let composedSubst = TypeOps.composeSubst state.Substitution newSubst
        { state with Substitution = composedSubst }

    /// Solve constraints using unification
    let solveConstraints (state: InferenceState) : InferenceState =
        try
            let solution = Unification.unifyConstraints state.Constraints
            { state with 
                Substitution = TypeOps.composeSubst state.Substitution solution
                Constraints = [] }
        with
        | Unification.UnificationException (Unification.TypeMismatch (t1, t2)) ->
            raise (TypeException (TypeMismatch (t1, t2, "Type unification failed")))
        | Unification.UnificationException (Unification.OccursCheck (var, ty)) ->
            raise (TypeException (OccursCheckFailure (var, ty)))
        | Unification.UnificationException (Unification.ArityMismatch (n1, n2)) ->
            raise (TypeException (ArityMismatch (n1, n2, "Tuple arity mismatch")))

    /// Type inference algorithm following syntax-directed rules
    let rec infer (env: TypeEnvironment) (expr: Expr) (state: InferenceState) : Type * InferenceState =
        match expr with
        | Literal lit ->
            // I-Lit rule
            let ty = TypeUtils.literalType lit
            (ty, state)

        | Variable x ->
            // I-Var rule with instantiation
            match TypeEnv.lookup env x with
            | Some scheme ->
                let ty = TypeOps.instantiate scheme
                (ty, state)
            | None ->
                raise (TypeException (UnboundVariable x))

        | Lambda (param, body) ->
            // I-Abs rule
            let (paramType, state) = freshTypeVar state
            let paramScheme = Forall ([], paramType)
            let extendedEnv = TypeEnv.extend env param paramScheme
            let (bodyType, state) = infer extendedEnv body state
            let functionType = TyArrow (paramType, bodyType)
            (functionType, state)

        | Application (func, arg) ->
            // I-App rule with unification
            let (funcType, state) = infer env func state
            let (argType, state) = infer env arg state
            let (resultType, state) = freshTypeVar state
            let expectedFuncType = TyArrow (argType, resultType)
            let state = addConstraint funcType expectedFuncType state
            let state = solveConstraints state
            let finalResultType = applyCurrentSubst resultType state
            (finalResultType, state)

        | Let (var, bindExpr, body) ->
            // I-Let rule with generalization
            let (bindType, state) = infer env bindExpr state
            let state = solveConstraints state
            let currentEnv = applyCurrentSubstEnv env state
            let finalBindType = applyCurrentSubst bindType state
            let scheme = TypeOps.generalize currentEnv finalBindType
            let extendedEnv = TypeEnv.extend currentEnv var scheme
            let (bodyType, state) = infer extendedEnv body state
            (bodyType, state)

        | LetRec (func, param, bindExpr, body) ->
            // I-Let-Rec rule
            let (funcType, state) = freshTypeVar state
            let (paramType, state) = freshTypeVar state
            let (resultType, state) = freshTypeVar state
            let expectedFuncType = TyArrow (paramType, resultType)
            
            // Add constraint that funcType should be a function type
            let state = addConstraint funcType expectedFuncType state
            
            // Create environments for type checking the recursive function
            let funcScheme = Forall ([], funcType)
            let paramScheme = Forall ([], paramType)
            let recEnv = TypeEnv.extend env func funcScheme
            let lambdaEnv = TypeEnv.extend recEnv param paramScheme
            
            // Infer type of function body
            let (bodyType, state) = infer lambdaEnv bindExpr state
            
            // Add constraint that body type matches result type
            let state = addConstraint bodyType resultType state
            let state = solveConstraints state
            
            // Generalize the function type for the body environment
            let currentEnv = applyCurrentSubstEnv env state
            let finalFuncType = applyCurrentSubst funcType state
            let generalizedScheme = TypeOps.generalize currentEnv finalFuncType
            let bodyEnv = TypeEnv.extend currentEnv func generalizedScheme
            
            // Infer type of the body expression
            let (finalBodyType, state) = infer bodyEnv body state
            (finalBodyType, state)

        | IfThenElse (cond, thenExpr, elseExpr) ->
            // I-If rule
            let (condType, state) = infer env cond state
            let (thenType, state) = infer env thenExpr state
            let (elseType, state) = infer env elseExpr state
            
            // Add constraints
            let state = addConstraint condType (TyCon TBool) state
            let state = addConstraint thenType elseType state
            let state = solveConstraints state
            
            let finalThenType = applyCurrentSubst thenType state
            (finalThenType, state)

        | Tuple exprs ->
            // I-Tup rule
            let rec inferTuple acc state = function
                | [] -> (List.rev acc, state)
                | expr :: rest ->
                    let (ty, state) = infer env expr state
                    inferTuple (ty :: acc) state rest
            
            let (types, state) = inferTuple [] state exprs
            let tupleType = TyTuple types
            (tupleType, state)

        | BinOp (op, left, right) ->
            // Binary operators
            let (leftType, state) = infer env left state
            let (rightType, state) = infer env right state
            let (argType1, argType2, resultType) = TypeUtils.binOpType op
            
            let state = addConstraint leftType argType1 state
            let state = addConstraint rightType argType2 state
            let state = solveConstraints state
            
            (resultType, state)

        | Sequence (expr1, expr2) ->
            // Sequence: type of first expression is ignored
            let (_, state) = infer env expr1 state
            let (type2, state) = infer env expr2 state
            (type2, state)

    /// Type check expression and return principal type
    let typeCheck (env: TypeEnvironment) (expr: Expr) : Type =
        let state = createState ()
        let (ty, state) = infer env expr state
        let state = solveConstraints state
        applyCurrentSubst ty state

    /// Type check with fresh environment
    let typeCheckFresh (expr: Expr) : Type =
        typeCheck (Builtins.initialTypeEnv ()) expr

    /// Type check and return both type and final environment
    let typeCheckWithEnv (env: TypeEnvironment) (expr: Expr) : Type * TypeEnvironment =
        let state = createState ()
        let (ty, state) = infer env expr state
        let state = solveConstraints state
        let finalType = applyCurrentSubst ty state
        let finalEnv = applyCurrentSubstEnv env state
        (finalType, finalEnv)

/// Type checking utilities
module TypeCheckUtils =
    
    /// Pretty print type error
    let printTypeError = function
        | UnboundVariable var ->
            sprintf "Unbound variable: %s" var
        | TypeMismatch (t1, t2, msg) ->
            sprintf "Type mismatch: expected %s but got %s\n%s" 
                (AstPrinter.printType t1) (AstPrinter.printType t2) msg
        | OccursCheckFailure (var, ty) ->
            sprintf "Occurs check failed: type variable '%c occurs in type %s" 
                (char (int 'a' + var % 26)) (AstPrinter.printType ty)
        | ArityMismatch (expected, actual, msg) ->
            sprintf "Arity mismatch: expected %d but got %d\n%s" expected actual msg
        | RecursiveTypeError msg ->
            sprintf "Recursive type error: %s" msg
        | InvalidRecursiveBinding msg ->
            sprintf "Invalid recursive binding: %s" msg

    /// Check if expression is well-typed
    let isWellTyped (env: TypeEnvironment) (expr: Expr) : bool =
        try
            let _ = TypeChecker.typeCheck env expr
            true
        with
        | TypeException _ -> false

    /// Get type of expression if well-typed
    let tryGetType (env: TypeEnvironment) (expr: Expr) : Type option =
        try
            Some (TypeChecker.typeCheck env expr)
        with
        | TypeException _ -> None

    /// Validate that recursive function is properly formed
    let validateRecursiveFunction (funcName: string) (param: string) (body: Expr) : bool =
        // Check that function name appears in the body (otherwise not recursive)
        let freeVars = AstUtils.freeVars body
        Set.contains funcName freeVars

    /// Get all type variables in an expression's type
    let getExpressionTypeVars (env: TypeEnvironment) (expr: Expr) : Set<int> =
        match tryGetType env expr with
        | Some ty -> AstUtils.freeTypeVars ty
        | None -> Set.empty

    /// Check if expression is polymorphic
    let isPolymorphic (env: TypeEnvironment) (expr: Expr) : bool =
        match tryGetType env expr with
        | Some ty -> TypeUtils.isPolymorphic ty
        | None -> false

    /// Get the most general type of an expression
    let getMostGeneralType (expr: Expr) : Type option =
        try
            Some (TypeChecker.typeCheckFresh expr)
        with
        | TypeException _ -> None

    /// Check type equivalence up to alpha-renaming
    let typeEquivalent (env1: TypeEnvironment) (expr1: Expr) (env2: TypeEnvironment) (expr2: Expr) : bool =
        match tryGetType env1 expr1, tryGetType env2 expr2 with
        | Some t1, Some t2 ->
            let nt1 = TypeUtils.normalizeType t1
            let nt2 = TypeUtils.normalizeType t2
            TypeOps.typeEqual nt1 nt2
        | _ -> false

/// Advanced type checking features
module AdvancedTypeCheck =
    
    /// Type inference with constraint collection (for debugging)
    let inferWithConstraints (env: TypeEnvironment) (expr: Expr) : Type * (Type * Type) list =
        let state = TypeChecker.createState ()
        let (ty, state) = TypeChecker.infer env expr state
        let constraints = state.Constraints
        try
            let state = TypeChecker.solveConstraints state
            let finalType = TypeChecker.applyCurrentSubst ty state
            (finalType, constraints)
        with
        | TypeException _ -> (ty, constraints)

    /// Incremental type checking for REPL
    type ReplState = {
        TypeEnvironment: TypeEnvironment
        EvaluationEnvironment: Environment
        History: (string * Type) list
    }

    let createReplState () : ReplState =
        {
            TypeEnvironment = Builtins.initialTypeEnv ()
            EvaluationEnvironment = Builtins.initialEvalEnv ()
            History = []
        }

    let addBinding (name: string) (expr: Expr) (state: ReplState) : ReplState * Type =
        try
            let ty = TypeChecker.typeCheck state.TypeEnvironment expr
            let scheme = TypeOps.generalize state.TypeEnvironment ty
            let newTypeEnv = TypeEnv.extend state.TypeEnvironment name scheme
            let newState = {
                state with
                    TypeEnvironment = newTypeEnv
                    History = (name, ty) :: state.History
            }
            (newState, ty)
        with
        | TypeException err ->
            failwith (TypeCheckUtils.printTypeError err)

    /// Type-directed code completion
    let getCompletions (env: TypeEnvironment) (prefix: string) : (string * TypeScheme) list =
        env
        |> TypeEnv.toList
        |> List.filter (fun (name, _) -> name.StartsWith(prefix))
        |> List.sortBy fst

    /// Find expressions of a given type
    let findExpressionsOfType (env: TypeEnvironment) (targetType: Type) : string list =
        env
        |> TypeEnv.toList
        |> List.choose (fun (name, scheme) ->
            let instType = TypeOps.instantiate scheme
            if TypeOps.typeEqual instType targetType then
                Some name
            else
                None)

    /// Type-based error suggestions
    let suggestCorrections (env: TypeEnvironment) (error: TypeError) : string list =
        match error with
        | UnboundVariable var ->
            let similar = 
                TypeEnv.domain env
                |> List.filter (fun name -> 
                    // Simple edit distance heuristic
                    abs (String.length name - String.length var) <= 2)
                |> List.take 3
            similar |> List.map (sprintf "Did you mean '%s'?")
        
        | TypeMismatch (expected, actual, _) ->
            let suggestions = findExpressionsOfType env expected
            if List.isEmpty suggestions then
                ["Check the type annotations and function applications"]
            else
                suggestions |> List.take 3 |> List.map (sprintf "Consider using '%s'")
        
        | _ -> ["Check the type system rules and constraints"]
    /// Get type of expression in REPL state
    let getReplExpressionType (state: ReplState) (expr: Expr)
        : Type option =
        try
            Some (TypeChecker.typeCheck state.TypeEnvironment expr)
        with
        | TypeException _ -> None
    /// Get type of expression in REPL state with environment
    let getReplExpressionTypeWithEnv (state: ReplState) (expr: Expr)
        : Type * TypeEnvironment option =
        try
            let ty = TypeChecker.typeCheck state.TypeEnvironment expr
            (ty, Some state.TypeEnvironment)
        with
        | TypeException _ -> (TyError, None)