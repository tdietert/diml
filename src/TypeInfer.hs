{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInfer where

import Syntax
import Type

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Identity

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type] 
  | Unallowed
  deriving (Show)
  
-- | Inference monad
type Infer a = (RWST
                  TypeEnv         -- Typing environment
                  [Constraint]    -- Generated constraints
                  InferState      -- Inference state
                  (Except         -- Inference errors
                    TypeError)
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

inferExpr :: TypeEnv -> DimlExpr -> Either TypeError Scheme
inferExpr env exp = 
    case runInfer env (infer exp) of
        Left err -> Left err
        Right (typ,constrs) ->
            case runSolver constrs of 
                Left err -> Left err
                Right sub -> Right $ generalize empty (apply sub typ)

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: TypeEnv -> DimlExpr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env exp = 
    case runInfer env (infer exp) of
        Left err -> Left err
        Right (typ,constrs) -> 
            case runSolver constrs of
                Left err -> Left err
                Right sub -> Right (constrs,sub,typ,generalize empty (apply sub typ))

-- | Substitution modeled with a newtype and Substitutable typeclass:
-- |    Substitutable typeclass defines functions apply and ftv for all
-- |    datatypes that will ever need a substitution applied over them

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ (TCon a)       = TCon a
    apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
    apply s (TArr t1 t2) = apply s t1 `TArr` apply s t2
    apply s (TProd t1 t2) = TProd (apply s t1) (apply s t2)

    ftv (TCon _)       = Set.empty
    ftv (TVar a)       = Set.singleton a
    ftv (TArr t1 t2)   = ftv t1 `Set.union` ftv t2
    ftv (TProd t1 t2)  = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply s' t
        where s' = Subst $ foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

-- | End subsitutable class definition
-- | ----------------------------------

typevars :: [String]
typevars = [1..] >>= flip replicateM ['a'..'z']

-- | Instantiation is the notion of giving fresh type variables to 
-- |     all variables in a given type scheme. This makes sure that when
-- |     looking up the type of a variable in a given environment
-- |     there are no type clashes. Maybe a bad idea?
-- | See http://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/let-gen.pdf
instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (\_ -> fresh) as
    let s = Subst $ Map.fromList (zip as as')
    return $ apply s t

-- | generalizing a type with respect to a given type env is akin to quantifying 
-- | the type variables that are free in type 't' but not in the type env
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

-- | Lookup type in the environment
lookupVarType :: Name -> Infer Type
lookupVarType x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable x
      -- gives fresh variable type variables in case of conflicts in env
      Just s@(Forall env typ) -> instantiate s >>= (\t -> return t)

-- "local" simply runs a function on the Reader Monad environment and
--  procduces the new Reader Environment resulting from the function execution
extendEnv :: [(Name, Scheme)] -> Infer a -> Infer a 
extendEnv schemes m = do 
    -- remove occurence of x from env, replace with new type
    let extEnv [] e = e
        extEnv ((x,sc):rest) e = extEnv rest newCtxt 
            where newCtxt = (e `remove` x) `extend` (x,sc)
    local (extEnv schemes) m

-- | Gives a fresh type variable
fresh :: Infer Type
fresh = do 
    st <- get 
    modify (\s -> s { count = count st + 1})
    return . TVar $ TV (typevars !! count st)

addConstr :: Type -> Type -> Infer ()
addConstr t1 t2 = tell [(t1,t2)]

opTypes :: Map.Map String Type
opTypes = Map.fromList [
        ("+", TArr tInt (TArr tInt tInt))
       ,("-", TArr tInt (TArr tInt tInt))
       ,("*", TArr tInt (TArr tInt tInt))
       ,("/", TArr tInt (TArr tInt tInt))
       ,(">", TArr tInt (TArr tInt tBool))
       ,("<", TArr tInt (TArr tInt tBool))
       ,("==",TArr tInt (TArr tInt tBool))
    ]

infer :: DimlExpr -> Infer Type
infer expr =
    case expr of
        Lit (DInt _) -> return tInt
        Lit (DFalse) -> return tBool
        Lit (DTrue)  -> return tBool 
        Var x        -> lookupVarType x

        BinOp op e1 e2 -> do 
            t1 <- infer e1
            t2 <- infer e2
            tv <- fresh
            let u1 = TArr t1 (TArr t2 tv)
                u2 = opTypes Map.! op
            addConstr u1 u2
            return tv

        Tuple e1 e2 ann -> do
            t1 <- infer e1
            t2 <- infer e2
            let tupType = TProd t1 t2
            case ann of 
                Just ann' -> do
                    addConstr ann' tupType
                Nothing -> return ()
            return tupType

        If e1 e2 e3 -> do
            t1 <- infer e1
            t2 <- infer e2
            t3 <- infer e3
            addConstr t1 tBool
            addConstr t2 t3
            return t2

        Decl x e -> infer e

        Apply e1 e2 -> do
            t1 <- infer e1
            t2 <- infer e2
            tv <- fresh
            addConstr t1 (TArr t2 tv)
            return tv

        Lam arg ann e -> do
            tvArg <- fresh
            tvRet <- fresh

            case ann of 
            	Just ann' -> do 
            		addConstr tvArg ann'
            	Nothing -> return ()

            -- adds argument to context with type tvArg to infer type of e
            t1 <- extendEnv [(arg, Forall [] tvArg)] (infer e)
            addConstr tvRet t1
            return $ TArr tvArg tvRet

        -- fix:
        -- | believe the programmer, make type annots 
        -- | influence the type inference here.
        Fun fname arg argAnn retAnn body -> do
            tvArg <- fresh
            tvRet <- fresh
            let funSch = Forall [] (TArr tvArg tvRet)
                argSch = Forall [] tvArg
            bodyType <- extendEnv [(fname,funSch),(arg,argSch)] (infer body)
            addConstr bodyType tvRet
            return $ TArr tvArg bodyType
        
        Let (Decl x e1) body -> do 
            t1 <- infer e1
            bodyType <- extendEnv [(x, Forall [] t1)] $ infer body
            return bodyType 
        Let fun@(Fun fname _ _ _ _) body -> do
            env <- ask
            funType <- infer fun
            let scheme = generalize env funType
            bodyType <- extendEnv [(fname, scheme)] $ infer body
            return bodyType
        
        PrintInt e1    -> do
            t1 <- infer e1
            addConstr t1 tInt
            return tInt

        Parens e1 ann -> do 
            t1 <- infer e1
            case ann of 
            	Just typ -> addConstr typ t1 >> return t1
                Nothing -> return t1
         
        Builtins e1 -> do
            case e1 of 
               TupFst e -> do
                   (TProd t1 t2) <- infer e
                   return t1
               TupSnd e -> do
                   (TProd t1 t2) <- infer e
                   return t2
 
-- | Unification algorithm

-- Solver carries around "unifier", a tuple of the current substitution
-- and constraints remaining to be solved. The Solver returns a final
-- substitution to be applied (see substitutable typeclass above)
-- to the resulting type from the constraint generation function "infer"
-- which produces a [Constraint] and Type as output
type Constraint = (Type,Type)
type Unifier = (Subst,[Constraint])
type Solver = ExceptT TypeError Identity 

runSolver :: [Constraint] -> Either TypeError Subst
runSolver cs = runIdentity $ runExceptT $ solve (emptySubst,cs)

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

emptySubst :: Subst
emptySubst = mempty

composeSubs :: Subst -> Subst -> Subst
composeSubs (Subst s1) (Subst s2) = Subst $ (Map.map (apply $ Subst s1) s2) `Map.union` s1

unify :: Type -> Type -> Solver Unifier
unify (TVar v) t = bindTVar v t
unify t (TVar v) = bindTVar v t 
unify (TArr t1 t2) (TArr t1' t2') = return $ (emptySubst, [(t1,t1'),(t2,t2')])
unify t1 t2 
    | t1 == t2 = return (emptySubst, [])
    | otherwise = throwError $ UnificationFail t1 t2

bindTVar :: TVar -> Type -> Solver Unifier
bindTVar a t 
    | occursCheck a t = throwError $ InfiniteType a t
    | otherwise = return $ (Subst (Map.singleton a t),[]) 

solve :: Unifier -> Solver Subst
solve (sub,[]) = return sub
solve (sub,((t1,t2):cs)) = do
    (newSub,constr) <- unify t1 t2
    solve (newSub `composeSubs` sub, constr ++ apply newSub cs)

occursCheck :: (Substitutable a) => TVar -> a -> Bool 
occursCheck x s = x `Set.member` ftv s
