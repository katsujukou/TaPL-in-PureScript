module TAPL.STLCEx.Types
  ( Ann
  , Name(..)
  , Prop(..)
  , Term(..)
  , Type_(..)
  , Var
  , module T
  , propKey
  , propValue
  , termAnn
  , typeAnn
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)
import TAPL.STLC.Syntax.Position (SourceRange)
import TAPL.STLC.Types (Ident) as T

data Name
  = NFree T.Ident
  | NBound Var

derive instance Eq Name 
derive instance Generic Name _ 
instance Show Name where
  show it = genericShow it 

data Prop a = Prop String a 

derive instance Functor Prop 
derive instance Eq a => Eq (Prop a)
derive instance Ord a => Ord (Prop a)
derive instance Generic (Prop a) _ 

instance Foldable Prop where
  foldMap f (Prop _ a) = f a
  foldl f a0 (Prop _ a) = f a0 a 
  foldr f a0 (Prop _ a) = f a a0

instance Traversable Prop where
  traverse f (Prop p a) = Prop p <$> f a
  sequence (Prop p a) = Prop p <$> a

instance Show a => Show (Prop a) where
  show it = genericShow it

propKey :: forall a. Prop a -> String
propKey (Prop k _) = k 

propValue :: forall a. Prop a -> a 
propValue (Prop _ a) = a 

data Type_ a 
  = TNat a
  | TBool a 
  | TFun a (Type_ a) (Type_ a)
  | TUnit a
  | TTuple a (Array (Type_ a))
  | TRecord a (Array (Prop (Type_ a)))

derive instance Functor Type_ 
derive instance Eq a => Eq (Type_ a)
derive instance Generic (Type_ a) _ 
instance Show a => Show (Type_ a) where
  show it = genericShow it

type Var = Int

data Pattern a
  = PatWildcard a 
  | PatIdent a T.Ident

derive instance Functor Pattern 
derive instance Eq a => Eq (Pattern a)
derive instance Generic (Pattern a) _ 
instance Show a => Show (Pattern a) where
  show it = genericShow it

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
  | TmLetIn a (Term a) (Term a)
  | TmUnit a
  | TmTuple a (Array (Term a))
  | TmRecord a (Array (Prop (Term a)))
  | TmField a (Term a) Int
  | TmProperty a (Term a) String

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
  TmUnit a -> a
  TmLetIn a _ _ -> a
  TmTuple a _ -> a 
  TmRecord a _ -> a
  TmField a _ _ -> a 
  TmProperty a _ _ -> a
  
typeAnn :: forall a. Type_ a -> a 
typeAnn = case _ of 
  TBool a -> a
  TNat a -> a 
  TFun a _ _ -> a
  TUnit a -> a 
  TTuple a _ -> a
  TRecord a _ -> a 