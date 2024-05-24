module TAPL.BoolNat.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Type_
  = TBool 
  | TNat 
  | TFun Type_ Type_ 

derive instance Eq Type_
derive instance Generic Type_ _ 
instance Show Type_ where
  show it = genericShow it

type Ident = String
type Var = Int

data Name 
  = NFree Ident
  | NBound Var

derive instance Eq Name 
derive instance Generic Name _ 
instance Show Name where
  show = genericShow

data Term a
  = TmTrue a
  | TmFalse a
  | TmIf a (Term a) (Term a) (Term a)
  | TmZero a
  | TmSucc a (Term a)
  | TmPred a (Term a)
  | TmIsZero a (Term a)
  -- | TmVar Var 
  -- | TmFree Name
  -- | TmAbs Type_ Term 
  -- | TmApp Term Term

derive instance Functor Term
derive instance Eq a => Eq (Term a) 
derive instance Generic (Term a) _ 
instance Show a => Show (Term a) where
  show it = genericShow it

isValue :: forall a. Term a -> Boolean
isValue = case _ of
  TmFalse _ -> true
  TmTrue _ -> true
  TmZero _ -> true
  TmSucc _ tm -> isValue tm
  _ -> false

termAnn :: forall a. Term a -> a 
termAnn = case _ of 
  TmFalse a -> a 
  TmTrue a -> a 
  TmIf a _ _ _ -> a
  TmZero a -> a
  TmSucc a _ -> a
  TmPred a _ -> a
  TmIsZero a _ -> a
