module Type (
    TVar(..),
    Type(..),
    Scheme(..),
    TypeEnv(..),
    tInt,
    tBool,
    extend,
    remove,
    empty
) where

import qualified Data.Map as Map

type Name = String

data TVar = TV String
    deriving (Eq, Ord, Show)

data Type
    = Unit
    | TVar TVar
    | TCon String
    | TArr Type Type
    | TProd Type Type
    | TSum Type Type
--  | TRec TVar Type
    deriving (Eq, Ord, Show)

data Scheme = Forall [TVar] Type
    deriving (Show)

tInt, tBool :: Type
tInt = TCon "Int"
tBool = TCon "Bool"

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x,s) = TypeEnv (Map.insert x s env)

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) x = TypeEnv (Map.delete x env)

empty :: TypeEnv
empty = TypeEnv Map.empty
