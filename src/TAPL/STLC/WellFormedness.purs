module TAPL.STLC.WellFormedness where


import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.List (List, (:))
import Data.List as L
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import TAPL.BoolNat.Syntax.Position ((~))
import TAPL.STLC.Error (Error(..))
import TAPL.STLC.Syntax.Types (Const(..), Expr(..), binderAnn, binderIdent)
import TAPL.STLC.Syntax.Types as Syntax
import TAPL.STLC.Types (Ann, Ident, Term(..), Type_(..), termAnn, typeAnn)

-- This module defines well-formedness checking functionality
-- After passing well-formedness checking, parsed terms (concrete syntax of source)
-- are transformed to AST terms and types, ready to be type-checked and evaluated.

type Env = List Ident 

type Check a = StateT Env (Except Error) a

runCheck :: forall a. Check a -> Either Error a
runCheck chk = runExcept $ evalStateT chk L.Nil 

extendEnv :: Ident -> Check Unit 
extendEnv var = do
  modify_ (var : _)

check :: Syntax.Expr Syntax.Ann -> Check (Term Ann)
check = case _ of 
  ExprConst a cst -> case cst of
    CstTrue -> pure $ TmTrue a 
    CstFalse -> pure $ TmFalse a
    CstNat n -> tailRecM convertNat (a /\ TmZero a /\ n)  
  ExprIf a cond ifso ifnot -> TmIf a
    <$> check cond
    <*> check ifso 
    <*> check ifnot
  ExprSucc a tm -> TmSucc a <$> check tm
  ExprIdent a ident -> case ident of 
    "isZero" -> throwError $ ExprIllFormed "isZero in unexpected position"
    "pred" -> throwError $ ExprIllFormed "pred in unexpected position"
    _ -> do 
      env <- get
      case List.findIndex (_ == ident) env of 
        Just i -> pure $ TmBound a i 
        Nothing -> throwError $ (UnknownIdentifier ident)
  ExprApp _ expAbs expArgs
    | [arg] <- NonEmptyArray.toArray expArgs
    , ExprIdent a ident <- expAbs -> case ident of 
        "pred" -> do
          tmArg <- check arg
          pure $ TmPred { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "isZero" -> do
          tmArg <- check arg
          pure $ TmIsZero { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        _ -> throwError $ UnknownIdentifier ident
    | otherwise -> do 
    tmAbs <- check expAbs 
    checkExprApp tmAbs (NonEmptyArray.toArray expArgs)
  ExprAbs a args body -> checkExprAbs a (NonEmptyArray.toArray args) body

  where
  -- convertNat :: a /\ Term a /\ Int -> Check (Step (a /\ Term a /\ Int) (Term a))
  convertNat (ann /\ prev /\ n) 
    | n == 0 = pure $ Done prev 
    | n > 0 = pure $ Loop (ann /\ TmSucc ann prev /\ (n - 1))
    | otherwise = throwError $ ExprIllFormed "Negative integer is not supported"

  checkExprApp tmAbs args = case Array.uncons args of 
    Nothing -> pure tmAbs
    Just { head, tail } -> do
      tmArg <- check head
      checkExprApp 
        (TmApp {pos:(termAnn tmAbs).pos ~ (termAnn tmArg).pos} tmAbs tmArg)
        tail

  checkExprAbs a args body = case Array.uncons args of
    Nothing -> check body
    Just { head, tail }  
      | Just typ <- Syntax.binderType head -> do 
          tmtyp <- checkType typ
          tmbody <- extendEnv (binderIdent head) *> checkExprAbs a tail body
          pure $ TmAbs a tmtyp tmbody
      | otherwise -> throwError $ BinderNotAnnotated { pos: (binderAnn head).pos }

  checkType = case _ of 
    Syntax.TFree a ident -> case ident of 
      "bool" -> pure $ TBool a
      "nat" -> pure $ TNat a
      _ -> throwError $ ExprIllFormed "Atomic types other than nat, bool are not supported."
    Syntax.TFun a argTypes retType -> checkFuncType a (NonEmptyArray.toArray argTypes) retType
    Syntax.TParens _ t -> checkType t
    
  checkFuncType a argTypes retType = case Array.uncons argTypes of 
    Nothing -> checkType retType
    Just { head: t, tail: ts } -> do
      tmTyp1 <- checkType t 
      tmTyp2 <- checkFuncType 
        {pos: (typeAnn tmTyp1).pos ~ a.pos} 
        ts 
        retType
      pure $ TFun {pos: a.pos} tmTyp1 tmTyp2