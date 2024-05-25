module TAPL.STLC.Types
  ( Ann
  , Term(..)
  , Type_(..)
  , Var
  , termAnn
  , typeAnn
  , module T
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TAPL.BoolNat.Syntax.Position (SourceRange)
import TAPL.BoolNat.Types (Ident) as T


data Type_ a 
  = TNat a
  | TBool a 
  | TFun a (Type_ a) (Type_ a)

derive instance Functor Type_ 
derive instance Eq a => Eq (Type_ a)
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
  | TmAbs a (Type_ a) (Term a)

derive instance Functor Term 
derive instance Generic (Term a) _
derive instance Eq a => Eq (Term a) 
instance Show a => Show (Term a) where
  show it = genericShow it 

type Ann = 
  { pos :: SourceRange
  }

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

typeAnn :: forall a. Type_ a -> a 
typeAnn = case _ of 
  TBool a -> a
  TNat a -> a 
  TFun a _ _ -> a 
