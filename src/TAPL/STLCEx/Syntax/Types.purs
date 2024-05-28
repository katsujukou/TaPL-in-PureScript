module TAPL.STLCEx.Syntax.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import TAPL.STLCEx.Syntax.Position (SourcePhrase, SourceRange)
import TAPL.STLCEx.Types (Ident)

-- This module declars definitions of parsed expressions (i.e. concrete syntax)  

data Token 
  = TokLeftParens
  | TokRightParens
  | TokLeftBrace 
  | TokRightBrace 
  | TokLeftSquare 
  | TokRightSquare
  | TokEqual
  | TokColon
  | TokSemicolon 
  | TokDot
  | TokComma
  | TokStar
  | TokRightArrow
  | TokUnit
  | TokUnderscore
  | TokReserved Keyword
  | TokIdent Ident
  | TokOperator String
  | TokIndex Int 
  | TokProperty String
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
  TokLeftBrace -> "{"
  TokRightBrace -> "}"
  TokLeftSquare -> "["
  TokRightSquare -> "]"
  TokEqual -> "="
  TokColon -> ":"
  TokSemicolon -> ";"
  TokDot -> "."
  TokComma -> ","
  TokStar -> "*"
  TokRightArrow -> "->"
  TokUnit -> "()"
  TokUnderscore -> "_"
  TokReserved kw -> printKeyword kw
  TokIdent ident -> ident
  TokOperator op -> op
  TokIndex idx -> "#" <> show idx
  TokProperty prop -> "#" <> prop
  TokBool b -> show b
  TokNat n -> show n 
  TokEOF -> ""
  where
    printKeyword = case _ of 
      KW_fun -> "fun"
      KW_let -> "let"
      KW_rec -> "rec"
      KW_and -> "and"
      KW_in -> "in"
      KW_case -> "case"
      KW_of -> "of"
      KW_if -> "if"
      KW_else -> "else"
      KW_then -> "then"  
      KW_as -> "as"

data Keyword
  = KW_fun 
  | KW_let 
  | KW_rec 
  | KW_in 
  | KW_and
  | KW_case 
  | KW_of
  | KW_if
  | KW_then 
  | KW_else
  | KW_as

derive instance Eq Keyword
derive instance Generic Keyword _ 
instance Show Keyword where 
  show = genericShow

type SourceToken = SourcePhrase Token

data Const
  = CstTrue 
  | CstFalse
  | CstNat Int
  | CstUnit

derive instance Eq Const 
derive instance Generic Const _ 
instance Show Const where
  show = genericShow

newtype Label = Label String

derive instance Eq Label 
derive instance Ord Label 
instance Show Label where 
  show (Label it) = "(Label " <> it <> ")"
  
data Labeled a = Labeled Label a

derive instance Functor Labeled 
derive instance Eq a => Eq (Labeled a)
derive instance Generic (Labeled a) _ 
instance Show a => Show (Labeled a) where
  show it = genericShow it 

data Type_ a
  = TFree a Ident
  | TFun a (NonEmptyArray (Type_ a)) (Type_ a)
  | TTup a (Array (Type_ a))
  | TRecord a (Array (Labeled (Type_ a)))
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
  TTup a _ -> a
  TRecord a _ -> a
  TParens a _ -> a

data Expr a
  = ExprConst a Const
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprSucc a (Expr a)
  | ExprIdent a Ident
  | ExprAbs a (NonEmptyArray (Binder a)) (Expr a)
  | ExprApp a (Expr a) (NonEmptyArray (Expr a))
  | ExprLet a (Binder a) (Expr a) (Expr a)
  | ExprLetRec a (NonEmptyArray { binder :: Binder a, expr :: Expr a }) (Expr a)
  | ExprTuple a (Array (Expr a))
  | ExprRecord a (Array (Labeled (Expr a)))
  | ExprAccess a (Expr a) (NonEmptyArray (Accessor a))
  | ExprAscription a (Expr a) (Type_ a)

derive instance Functor Expr 
derive instance Eq a => Eq (Expr a) 
derive instance Generic (Expr a) _ 
instance Show a => Show (Expr a) where
  show it = genericShow it

data Accessor a 
  = AcsIndex a Int
  | AcsProperty a Label

derive instance Functor Accessor
derive instance Eq a => Eq (Accessor a)
derive instance Generic (Accessor a) _ 
instance Show a => Show(Accessor a) where
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
  ExprLetRec a _ _ -> a
  ExprTuple a _ -> a
  ExprAccess a _ _ -> a
  ExprRecord a _ -> a 
  ExprAscription a _ _ -> a
patternAnn :: forall a. Pattern a -> a 
patternAnn = case _ of 
  PatVar a _ -> a 
  PatWildcard a -> a 

patternVar :: forall a. Pattern a -> Maybe Ident
patternVar = case _ of 
  PatVar _ var -> Just var
  _ -> Nothing

accessorAnn :: forall a. Accessor a -> a 
accessorAnn = case _ of 
  AcsIndex a _ -> a 
  AcsProperty a _ -> a
