module TAPL.BoolNat.Syntax.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import TAPL.BoolNat.Syntax.Position (SourcePhrase, SourceRange)
import TAPL.BoolNat.Types (Ident)

-- This module declars definitions of parsed expressions (i.e. concrete syntax)  

data Token 
  = TokLeftParens
  | TokRightParens
  | TokReserved Keyword
  | TokIdent Ident
  | TokBool Boolean
  | TokNat Int
  | TokEOF

derive instance Eq Token 
derive instance Generic Token _ 

instance Show Token where
  show = genericShow 

printToken :: Token -> String
printToken = case _ of 
  TokLeftParens -> "("
  TokRightParens -> ")"
  TokReserved kw -> printKeyword kw
  TokIdent ident -> ident
  TokBool b -> show b
  TokNat n -> show n 
  TokEOF -> ""
  where
    printKeyword = case _ of 
      KW_if -> "if"
      KW_else -> "else"
      KW_then -> "then"  
    
data Keyword
  = KW_if
  | KW_then 
  | KW_else

derive instance Eq Keyword
derive instance Generic Keyword _ 
instance Show Keyword where 
  show = genericShow

type SourceToken = SourcePhrase Token

data Const
  = CstTrue 
  | CstFalse
  | CstNat Int

derive instance Eq Const 
derive instance Generic Const _ 
instance Show Const where
  show = genericShow

data Expr a
  = ExprConst a Const
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprSucc a (Expr a)
  | ExprIdent a Ident
  | ExprApp a (Expr a) (NonEmptyArray (Expr a))

derive instance Functor Expr 
derive instance Foldable Expr 
derive instance Traversable Expr
derive instance Eq a => Eq (Expr a) 
derive instance Generic (Expr a) _ 
instance Show a => Show (Expr a) where
  show it = genericShow it

type Ann =
  { pos :: SourceRange }

exprAnn :: forall a. Expr a -> a 
exprAnn = case _ of
  ExprConst a _ -> a
  ExprIf a _ _ _ -> a
  ExprSucc a _ -> a
  ExprIdent a _ -> a
  ExprApp a _ _ -> a
