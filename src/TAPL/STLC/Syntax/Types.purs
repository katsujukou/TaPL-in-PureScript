module TAPL.STLC.Syntax.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import TAPL.STLC.Syntax.Position (SourcePhrase, SourceRange)
import TAPL.STLC.Types (Ident)

-- This module declars definitions of parsed expressions (i.e. concrete syntax)  

data Token 
  = TokLeftParens
  | TokRightParens
  | TokEqual
  | TokColon
  | TokRightArrow
  | TokReserved Keyword
  | TokIdent Ident
  | TokOperator String
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
  TokEqual -> "="
  TokColon -> ":"
  TokRightArrow -> "->"
  TokReserved kw -> printKeyword kw
  TokIdent ident -> ident
  TokOperator op -> op
  TokBool b -> show b
  TokNat n -> show n 
  TokEOF -> ""
  where
    printKeyword = case _ of 
      KW_fun -> "fun"
      KW_if -> "if"
      KW_else -> "else"
      KW_then -> "then"  
    
data Keyword
  = KW_fun 
  | KW_if
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

data Type_ a
  = TFree a Ident
  | TFun a (NonEmptyArray (Type_ a)) (Type_ a)
  | TParens a (Type_ a)
  
derive instance Functor Type_ 
derive instance Eq a => Eq (Type_ a) 
derive instance Generic (Type_ a) _ 
instance Show a => Show (Type_ a) where
  show it = genericShow it 

typeAnn :: forall a. Type_ a -> a 
typeAnn = case _ of 
  TFree a _ -> a 
  TFun a _ _ -> a 
  TParens a _ -> a

data Expr a
  = ExprConst a Const
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprSucc a (Expr a)
  | ExprIdent a Ident
  | ExprAbs a (NonEmptyArray (Binder a)) (Expr a)
  | ExprApp a (Expr a) (NonEmptyArray (Expr a))

derive instance Functor Expr 
derive instance Eq a => Eq (Expr a) 
derive instance Generic (Expr a) _ 
instance Show a => Show (Expr a) where
  show it = genericShow it

data Binder a = Binder a Ident (Maybe (Type_ a))

derive instance Functor Binder 
derive instance Eq a => Eq (Binder a)
derive instance Generic (Binder a) _ 
instance Show a => Show (Binder a) where
  show = genericShow

binderIdent :: forall a. Binder a -> Ident 
binderIdent (Binder _ id _) = id

binderType :: forall a. Binder a -> Maybe (Type_ a)
binderType (Binder _ _ typ) = typ 

type Ann =
  { pos :: SourceRange }

exprAnn :: forall a. Expr a -> a 
exprAnn = case _ of
  ExprConst a _ -> a
  ExprIf a _ _ _ -> a
  ExprSucc a _ -> a
  ExprIdent a _ -> a
  ExprAbs a _ _ -> a
  ExprApp a _ _ -> a

binderAnn :: forall a. Binder a -> a 
binderAnn (Binder a _ _) = a