module TAPL.TRecon.Syntax.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import TAPL.TRecon.Syntax.Position (SourcePhrase, SourceRange)
import TAPL.TRecon.Types (Ident)

-- This module declars definitions of parsed expressions (i.e. concrete syntax)  

data Token 
  = TokLeftParens
  | TokRightParens
  | TokEqual
  | TokColon
  | TokUnderscore
  | TokRightArrow
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
  TokEqual -> "="
  TokColon -> ":"
  TokUnderscore -> "_"
  TokRightArrow -> "->"
  TokReserved kw -> printKeyword kw
  TokIdent ident -> ident
  TokBool b -> show b
  TokNat n -> show n 
  TokEOF -> ""
  where
    printKeyword = case _ of 
      KW_fun -> "fun"
      KW_if -> "if"
      KW_else -> "else"
      KW_then -> "then"  
      KW_let -> "let"
      KW_in -> "in"
    
data Keyword
  = KW_fun 
  | KW_if
  | KW_then 
  | KW_else
  | KW_let 
  | KW_in
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
  | ExprLet a (Binder a) (Expr a) (Expr a)

derive instance Functor Expr 
derive instance Eq a => Eq (Expr a) 
derive instance Generic (Expr a) _ 
instance Show a => Show (Expr a) where
  show it = genericShow it

data Binder a = Binder a (Pattern a) (Maybe (Type_ a))

derive instance Functor Binder 
derive instance Eq a => Eq (Binder a)
derive instance Generic (Binder a) _
instance Show a => Show (Binder a) where
  show it = genericShow it

binderAnn :: forall a. Binder a -> a
binderAnn (Binder a _ _) = a 

binderPattern :: forall a. Binder a -> Pattern a 
binderPattern (Binder _ p _) = p 

binderType :: forall a. Binder a -> Maybe (Type_ a)
binderType (Binder _ _ typ) = typ 

isWildcardBinder :: forall a. Binder a -> Boolean
isWildcardBinder (Binder _ (PatWildcard _) _) = true 
isWildcardBinder _ = false 

data Pattern a 
  = PatVar a Ident
  | PatWildcard a 

derive instance Functor Pattern
derive instance Eq a => Eq (Pattern a)
derive instance Generic (Pattern a) _ 
instance Show a => Show (Pattern a) where
  show it = genericShow it

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
  ExprLet a _ _ _ -> a

patternAnn :: forall a. Pattern a -> a 
patternAnn = case _ of 
  PatVar a _ -> a 
  PatWildcard a -> a 