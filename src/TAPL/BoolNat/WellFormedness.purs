module BoolNat.Syntax.WellFormedness where


import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import TAPL.BoolNat.Error (Error(..))
import TAPL.BoolNat.Result (Result, throwError)
import TAPL.BoolNat.Syntax.Types (Const(..), Expr(..), exprAnn)
import TAPL.BoolNat.Types (Term(..))

-- This module defines well-formedness checking functionality
-- After passing well-formedness checking, parsed terms (concrete syntax of source)
-- are transformed to AST terms and types, ready to be type-checked and evaluated.

check :: forall a. Expr a -> Result (Term a)
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
  -- ExprPred _ tm -> TmPred <$> check tm 
  -- ExprIsZero _ tm -> TmIsZero <$> check tm
  ExprIdent _ ident -> case ident of 
    "isZero" -> throwError $ ExprIllFormed "isZero in unexpected position"
    "pred" -> throwError $ ExprIllFormed "pred in unexpected position"
    _ -> throwError $ (UnknownIdentifier ident)
  -- The ExprApp Case is included for completeness, but within the boolean arithmetic
  -- any application is not regarded as well-formed.
  ExprApp _ expAbs expArgs
    | {head: expArg, tail: expRest } <- NonEmptyArray.uncons expArgs -> do
        tmHead <- checkHead expAbs expArg
        tailRecM (elimAppSpine tmHead) expRest 

  where
  convertNat :: a /\ Term a /\ Int -> Result (Step (a /\ Term a /\ Int) (Term a))
  convertNat (ann /\ prev /\ n) 
    | n == 0 = pure $ Done prev 
    | n > 0 = pure $ Loop (ann /\ TmSucc ann prev /\ (n - 1))
    | otherwise = throwError $ ExprIllFormed "Negative integer is not supported"

  checkHead abs arg = case abs of 
    ExprIdent a ident -> case ident of 
      "pred" -> TmPred a  <$> check arg 
      "isZero" -> TmIsZero a <$> check arg 
      _ -> throwError $ UnknownIdentifier ident
    _ -> throwError $ IllegalApplication  

  elimAppSpine exp = Array.uncons >>> case _ of 
    Nothing -> pure $ Done exp 
    Just _ -> do 
      -- tmHead <- check head
      throwError $ IllegalApplication
      -- pure $ Loop (TmApp ) tail