/// Type system operations for TinyML
/// Implementation of Hindley-Milner type system operations
namespace TinyML

open TinyML

/// Type operations and utilities
module TypeOps =
    
    /// Apply substitution to a type
    let rec applySubst (subst: Substitution) (ty: Type) : Type =
        match ty with
        | TyCon _ -> ty
        | TyVar n -> 
            match Map.tryFind n subst with
            | Some t -> t
            | None -> ty
        | TyArrow (t1, t2) -> TyArrow (applySubst subst t1, applySubst subst t2)
        | TyTuple types -> TyTuple (List.map (applySubst subst) types)

    /// Apply substitution to a type scheme
    let applySubstScheme (subst: Substitution) (Forall (bound, ty)) : TypeScheme =
        // Remove bound variables from substitution to avoid capture
        let filteredSubst = 
            bound 
            |> List.fold (fun acc var -> Map.remove var acc) subst
        Forall (bound, applySubst filteredSubst ty)

    /// Apply substitution to a type environment
    let applySubstEnv (subst: Substitution) (env: TypeEnvironment) : TypeEnvironment =
        env |> Map.map (fun _ scheme -> applySubstScheme subst scheme)

    /// Compose two substitutions: (s2 ∘ s1)
    let composeSubst (s1: Substitution) (s2: Substitution) : Substitution =
        // Apply s2 to the codomain of s1, then union with s2
        let s1Applied = s1 |> Map.map (fun _ ty -> applySubst s2 ty)
        Map.fold (fun acc k v -> Map.add k v acc) s1Applied s2

    /// Create singleton substitution [α ↦ τ]
    let singletonSubst (var: int) (ty: Type) : Substitution =
        Map.singleton var ty

    /// Empty substitution
    let emptySubst : Substitution = Map.empty

    /// Occurs check: does type variable occur in type?
    let rec occurs (var: int) (ty: Type) : bool =
        match ty with
        | TyCon _ -> false
        | TyVar n -> n = var
        | TyArrow (t1, t2) -> occurs var t1 || occurs var t2
        | TyTuple types -> List.exists (occurs var) types

    /// Generalization: gen_Γ(τ) = ∀α.τ where α = ftv(τ) \ ftv(Γ)
    let generalize (env: TypeEnvironment) (ty: Type) : TypeScheme =
        let envFreeVars = AstUtils.freeTypeVarsEnv env
        let tyFreeVars = AstUtils.freeTypeVars ty
        let quantified = Set.difference tyFreeVars envFreeVars |> Set.toList
        Forall (quantified, ty)

    /// Instantiation: inst(∀α.τ) = re_α(τ) with fresh type variables
    let instantiate (scheme: TypeScheme) : Type =
        match scheme with
        | Forall ([], ty) -> ty
        | Forall (bound, ty) ->
            // Create fresh type variables for bound variables
            let freshSubst = 
                bound 
                |> List.map (fun var -> (var, AstUtils.freshTypeVar()))
                |> Map.ofList
            applySubst freshSubst ty

    /// Check if two types are equal (α-equivalent)
    let rec typeEqual (t1: Type) (t2: Type) : bool =
        match t1, t2 with
        | TyCon c1, TyCon c2 -> c1 = c2
        | TyVar n1, TyVar n2 -> n1 = n2
        | TyArrow (a1, b1), TyArrow (a2, b2) -> typeEqual a1 a2 && typeEqual b1 b2
        | TyTuple ts1, TyTuple ts2 -> 
            List.length ts1 = List.length ts2 && 
            List.forall2 typeEqual ts1 ts2
        | _ -> false

/// Most General Unifier implementation
module Unification =
    
    open TypeOps

    /// Unification error
    type UnificationError =
        | TypeMismatch of Type * Type
        | OccursCheck of int * Type
        | ArityMismatch of int * int

    exception UnificationException of UnificationError

    /// Unify two types: U(τ₁, τ₂) = θ such that θ(τ₁) ≡ θ(τ₂)
    let rec unify (t1: Type) (t2: Type) : Substitution =
        if typeEqual t1 t2 then
            emptySubst
        else
            match t1, t2 with
            | TyVar n, ty | ty, TyVar n ->
                if occurs n ty then
                    raise (UnificationException (OccursCheck (n, ty)))
                else
                    singletonSubst n ty
            
            | TyCon c1, TyCon c2 when c1 = c2 -> emptySubst
            
            | TyArrow (a1, b1), TyArrow (a2, b2) ->
                let s1 = unify a1 a2
                let s2 = unify (applySubst s1 b1) (applySubst s1 b2)
                composeSubst s1 s2
            
            | TyTuple ts1, TyTuple ts2 ->
                if List.length ts1 <> List.length ts2 then
                    raise (UnificationException (ArityMismatch (List.length ts1, List.length ts2)))
                else
                    unifyList ts1 ts2
            
            | _ -> raise (UnificationException (TypeMismatch (t1, t2)))

    /// Unify a list of type pairs
    and unifyList (types1: Type list) (types2: Type list) : Substitution =
        match types1, types2 with
        | [], [] -> emptySubst
        | t1::ts1, t2::ts2 ->
            let s1 = unify t1 t2
            let s2 = unifyList (List.map (applySubst s1) ts1) (List.map (applySubst s1) ts2)
            composeSubst s1 s2
        | _ -> failwith "Lists of different lengths in unifyList"

    /// Unify multiple constraints
    let unifyConstraints (constraints: (Type * Type) list) : Substitution =
        constraints
        |> List.fold (fun subst (t1, t2) ->
            let t1' = applySubst subst t1
            let t2' = applySubst subst t2
            let newSubst = unify t1' t2'
            composeSubst subst newSubst
        ) emptySubst

/// Utility functions for working with types
module TypeUtils =
    
    open TypeOps

    /// Create function type
    let mkArrow arg ret = TyArrow (arg, ret)

    /// Create tuple type
    let mkTuple types = 
        match types with
        | [] -> TyCon TUnit
        | [t] -> t
        | _ -> TyTuple types

    /// Get the type of a literal
    let literalType = function
        | LInt _ -> TyCon TInt
        | LFloat _ -> TyCon TFloat
        | LBool _ -> TyCon TBool
        | LString _ -> TyCon TString
        | LChar _ -> TyCon TChar
        | LUnit -> TyCon TUnit

    /// Get the result type of a binary operation
    let binOpType = function
        | Add | Sub | Mul | Div | Mod -> (TyCon TInt, TyCon TInt, TyCon TInt)
        | Eq | Ne | Lt | Le | Gt | Ge -> (TyCon TInt, TyCon TInt, TyCon TBool)
        | And | Or -> (TyCon TBool, TyCon TBool, TyCon TBool)

    /// Check if a type is a function type
    let isFunctionType = function
        | TyArrow _ -> true
        | _ -> false

    /// Extract argument and return types from function type
    let extractArrowType = function
        | TyArrow (arg, ret) -> Some (arg, ret)
        | _ -> None

    /// Get arity of a type (number of arguments for function types)
    let rec arity = function
        | TyArrow (_, ret) -> 1 + arity ret
        | _ -> 0

    /// Curry a function type: (a * b * c) -> d becomes a -> b -> c -> d
    let rec curryType = function
        | TyArrow (TyTuple args, ret) ->
            List.foldBack (fun arg acc -> TyArrow (arg, acc)) args ret
        | ty -> ty

    /// Uncurry a function type: a -> b -> c -> d becomes (a * b * c) -> d
    let rec uncurryType = function
        | TyArrow (arg, ret) ->
            let args, finalRet = collectArgs [arg] ret
            if List.length args > 1 then
                TyArrow (TyTuple args, finalRet)
            else
                TyArrow (arg, ret)
        | ty -> ty
    
    and collectArgs acc = function
        | TyArrow (arg, ret) -> collectArgs (arg :: acc) ret
        | ty -> (List.rev acc, ty)

    /// Check if a type contains type variables
    let rec isPolymorphic = function
        | TyVar _ -> true
        | TyArrow (t1, t2) -> isPolymorphic t1 || isPolymorphic t2
        | TyTuple types -> List.exists isPolymorphic types
        | TyCon _ -> false

    /// Get all type variables in a type
    let rec getTypeVars = function
        | TyVar n -> [n]
        | TyArrow (t1, t2) -> getTypeVars t1 @ getTypeVars t2
        | TyTuple types -> List.collect getTypeVars types
        | TyCon _ -> []

    /// Normalize type variable names (rename to consecutive integers)
    let normalizeType (ty: Type) : Type =
        let vars = getTypeVars ty |> List.distinct |> List.sort
        let mapping = vars |> List.mapi (fun i var -> (var, i)) |> Map.ofList
        let rec normalize = function
            | TyVar n -> TyVar (Map.find n mapping)
            | TyArrow (t1, t2) -> TyArrow (normalize t1, normalize t2)
            | TyTuple types -> TyTuple (List.map normalize types)
            | TyCon c -> TyCon c
        normalize ty