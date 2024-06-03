module TAPL.TRecon.Types
  ( Ann
  , Term(..)
  , Type_(..)
  , Var
  , emptyAnn
  , module T
  , setTypeAnn
  , termAnn
  , typEq
  , typeAnn
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import TAPL.BoolNat.Syntax.Position (SourceRange)
import TAPL.BoolNat.Types (Ident) as T
import TAPL.TRecon.Syntax.Position (emptyRange)


data Type_ a 
  = TNat a
  | TBool a 
  | TFun a (Type_ a) (Type_ a)
  | TUnit a
  | TVar a T.Ident

derive instance Functor Type_ 
derive instance Eq a => Eq (Type_ a)
derive instance Ord a => Ord (Type_ a)
derive instance Generic (Type_ a) _ 
instance Show a => Show (Type_ a) where
  show it = genericShow it

type Var = Int

data Term a 
  = TmTrue a
  | TmFalse a 
  | TmIf a (Term a) (Term a) (Term a)
  | TmZero a 
  | TmSucc a (Term a)
  | TmPred a (Term a)
  | TmIsZero a (Term a)
  | TmBound a Var
  | TmApp a (Term a) (Term a)
  | TmAbs a (Maybe (Type_ a)) (Term a)
  | TmUnit a
  | TmLetIn a (Maybe (Type_ a)) (Term a) (Term a)

derive instance Functor Term 
derive instance Generic (Term a) _
derive instance Eq a => Eq (Term a) 
instance Show a => Show (Term a) where
  show it = genericShow it 

type Ann = 
  { pos :: SourceRange
  }

emptyAnn :: Ann 
emptyAnn = { pos: emptyRange }

termAnn :: forall a. Term a -> a 
termAnn = case _ of 
  TmTrue a -> a
  TmFalse a -> a
  TmIf a _ _ _ -> a 
  TmZero a -> a
  TmSucc a _ -> a 
  TmPred a _ -> a 
  TmIsZero a _ -> a 
  TmBound a _ -> a
  TmApp a _ _ -> a 
  TmAbs a _ _ -> a
  TmUnit a -> a
  TmLetIn a _ _ _ -> a

typeAnn :: forall a. Type_ a -> a 
typeAnn = case _ of 
  TBool a -> a
  TNat a -> a 
  TFun a _ _ -> a 
  TUnit a -> a
  TVar a _ -> a

setTypeAnn :: forall a b. a -> Type_ b -> Type_ a
setTypeAnn a = map (const a)


-- Equality test for types up to annotations.
typEq :: forall a b. Type_ a -> Type_ b -> Boolean
typEq ty1 ty2 = (setTypeAnn unit ty1) == (setTypeAnn unit ty2)
