module TAPL.TRecon.WellFormedness where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Data.Array (foldl)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import TAPL.BoolNat.Syntax.Position ((~))
import TAPL.TRecon.Env (VarEnv(..), Occurrence)
import TAPL.TRecon.Env as Env
import TAPL.TRecon.Error (Error(..))
import TAPL.TRecon.Syntax.Types (Const(..), Expr(..))
import TAPL.TRecon.Syntax.Types as Syntax
import TAPL.TRecon.Types (Ident, Term(..), Type_(..), Ann, termAnn, typeAnn)

-- This module defines well-formedness checking functionality
-- After passing well-formedness checking, parsed terms (concrete syntax of source)
-- are transformed to AST terms and types, ready to be type-checked and evaluated.

type CheckState =
  { freshVar :: Int
  }

type Check a = ReaderT Env.VarEnv (StateT CheckState (Except Error)) a

runCheck :: forall a. Check a -> Either Error a
runCheck chk = runExcept $ evalStateT (runReaderT chk VEnvNull) { freshVar: 0 } 

freshVar :: Check Ident
freshVar = do
  { freshVar: fv } <- get 
  modify_ (\s -> s { freshVar = s.freshVar + 1 })
  pure $ "$" <> show (fv + 1)

withExtendEnv :: forall a b. Syntax.Binder a -> Check b -> Check b 
withExtendEnv (Syntax.Binder _ pat _) m = do
  vars <- patVars pat
  local (extend vars) m
  where
    extend :: Array (Ident /\ Occurrence) -> Env.VarEnv -> Env.VarEnv
    extend vars env = foldl 
      (\env0 (ident /\ o) -> Env.extendVarEnvNew env0 ident o) 
      env
      vars

    patVars :: Syntax.Pattern _ -> _ (Array (Ident /\ Occurrence))
    patVars = case _ of
      Syntax.PatVar _ ident -> pure [ident /\ Nil] 
      Syntax.PatWildcard _ -> do 
        unused <- freshVar
        pure [unused /\ Nil]

check :: Syntax.Expr Syntax.Ann -> Check (Term Ann)
check = case _ of 
  ExprConst a cst -> case cst of
    CstTrue -> pure $ TmTrue a 
    CstFalse -> pure $ TmFalse a
    CstNat n -> tailRecM convertNat (a /\ TmZero a /\ n)  
    CstUnit -> pure $ TmUnit a
  ExprIf a cond ifso ifnot -> TmIf a
    <$> check cond
    <*> check ifso 
    <*> check ifnot
  ExprSucc a tm -> TmSucc a <$> check tm
  ExprIdent a ident -> case ident of 
    "isZero" -> throwError $ ExprIllFormed "isZero in unexpected position"
    "pred" -> throwError $ ExprIllFormed "pred in unexpected position"
    "succ" -> throwError $ ExprIllFormed "succ in unexpected position"
    -- "fix" -> throwError $ ExprIllFormed "fix in unexpected position"
    _ -> do 
      env <- ask
      case Env.searchVarEnv ident env of 
        Just (i /\ _) -> pure $ TmBound a i 
        Nothing -> throwError $ (UnknownIdentifier ident)
  ExprApp _ expAbs expArgs
    | [arg] <- NonEmptyArray.toArray expArgs
    , ExprIdent a ident <- expAbs -> case ident of 
        "pred" -> do
          tmArg <- check arg
          pure $ TmPred { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "succ" -> do
          tmArg <- check arg
          pure $ TmSucc { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "isZero" -> do
          tmArg <- check arg
          pure $ TmIsZero { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        _ -> do 
          env <- ask
          case Env.searchVarEnv ident env of 
            Just (i /\ _) -> checkExprApp (TmBound a i) [arg]
            Nothing -> throwError $ (UnknownIdentifier ident)
    | otherwise -> do 
      tmAbs <- check expAbs 
      checkExprApp tmAbs (NonEmptyArray.toArray expArgs)
  ExprAbs a args body -> checkExprAbs a (NonEmptyArray.toArray args) body
  ExprLet a pat e1 e2 -> checkExprLet a pat e1 e2 

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
    Just { head, tail } -> do
      mbtyp <- traverse checkType $ Syntax.binderType head
      withExtendEnv head do
        tmbody <- checkExprAbs a tail body
        pure $ TmAbs a mbtyp tmbody

  checkExprLet a binder e1 e2 = do 
    mbtyp <- traverse checkType $ Syntax.binderType binder
    tm1 <- check e1
    tm2 <- withExtendEnv binder do
      check e2
    pure $ TmLetIn a mbtyp tm1 tm2

  checkType = case _ of 
    Syntax.TFree a ident -> case ident of 
      "bool" -> pure $ TBool a
      "nat" -> pure $ TNat a
      "unit" -> pure $ TUnit a
      _ -> pure $ TVar a ident
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