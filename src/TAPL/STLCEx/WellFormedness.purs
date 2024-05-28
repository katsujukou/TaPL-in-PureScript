module TAPL.STLCEx.WellFormedness where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, modify)
import Data.Array (foldl)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import TAPL.BoolNat.Syntax.Position ((~))
import TAPL.STLCEx.Env (Occurrence, VarEnv(..))
import TAPL.STLCEx.Env as Env
import TAPL.STLCEx.Error (Error(..))
import TAPL.STLCEx.Syntax.Types (Accessor(..), Const(..), Expr(..), Pattern(..))
import TAPL.STLCEx.Syntax.Types as Syntax
import TAPL.STLCEx.Syntax.Utils (isUppercase)
import TAPL.STLCEx.Types (Ann, Ident, Prop(..), Term(..), Type_(..), emptyAnn, termAnn, typeAnn)

-- This module defines well-formedness checking functionality
-- After passing well-formedness checking, parsed terms (concrete syntax of source)
-- are transformed to AST terms and types, ready to be type-checked and evaluated.

type Check a = ReaderT Env.VarEnv (StateT Int (Except Error)) a

runCheck :: forall a. Check a -> Either Error a
runCheck chk = runExcept $ evalStateT (runReaderT chk VEnvNull) 0 

freshVar :: Check Ident
freshVar = do
  fresh <- modify (_ + 1)
  pure $ "$" <> show fresh

withExtendEnv :: forall a b. Syntax.Binder a -> Check b -> Check b 
withExtendEnv (Syntax.Binder _ pat _) m = do
  vars <- patVars pat
  local (extend vars) m
  where
    extend :: Array (Ident /\ Occurrence) -> Env.VarEnv -> Env.VarEnv
    extend vars env = foldl 
      (\env0 (ident/\o) -> Env.extendVarEnvNew env0 ident o) 
      env
      vars

    patVars :: Syntax.Pattern _ -> _ (Array (Ident /\ Occurrence))
    patVars = case _ of
      PatVar _ ident -> pure [ident /\ Nil] 
      PatWildcard _ -> do 
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
    _ -> do 
      env <- ask
      case Env.searchVarEnv ident env of 
        Just (i /\ _) -> pure $ TmBound a i 
        Nothing -> 
          let _ = unsafePerformEffect (logShow env) 
          in throwError $ (UnknownIdentifier ident)
  ExprApp _ expAbs expArgs
    | [arg] <- NonEmptyArray.toArray expArgs
    , ExprIdent a ident <- expAbs -> case ident of 
        "pred" -> do
          tmArg <- check arg
          pure $ TmPred { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "isZero" -> do
          tmArg <- check arg
          pure $ TmIsZero { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        _ -> do 
          env <- ask
          case Env.searchVarEnv ident env of 
            Just (i /\ _) -> do 
              checkExprApp (TmBound a i) (NonEmptyArray.toArray expArgs)
            _ -> throwError $ UnknownIdentifier ident
    | otherwise -> do 
    tmAbs <- check expAbs 
    checkExprApp tmAbs (NonEmptyArray.toArray expArgs)
  ExprAbs a args body -> checkExprAbs a (NonEmptyArray.toArray args) body
  ExprLet a binder e1 e2 -> checkExprLet a binder e1 e2 
  ExprTuple a elems -> checkExprTuple a elems
  ExprRecord a lblExprs -> checkExprRecord a lblExprs
  ExprAccess _ exp path -> do
    tm <- check exp 
    checkExprAccess tm (NonEmptyArray.toArray path)
  ExprAscription a exp typ -> do
    -- Desugar `as` derived form into application
    -- tm as typ == (fun (x:typ) -> x) tm
    abs <- do
      var <- freshVar
      pure $ ExprAbs 
        emptyAnn
        (NonEmptyArray.singleton (Syntax.Binder emptyAnn (Syntax.PatVar emptyAnn var) (Just typ)))
        (Syntax.ExprIdent emptyAnn var)
    check (ExprApp a abs (NonEmptyArray.singleton exp))
  _ -> throwError $ ExprIllFormed "not implemented"
  where
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
          withExtendEnv head do
            tmtyp <- checkType typ
            tmbody <- checkExprAbs a tail body
            pure $ TmAbs a tmtyp tmbody
      | otherwise -> throwError $ BinderNotAnnotated { pos: (Syntax.binderAnn head).pos }

  checkExprLet a binder e1 e2 = do 
    tm1 <- check e1
    tm2 <- withExtendEnv binder do
      check e2
    pure $ TmLetIn a tm1 tm2
  
  checkExprTuple a elems = do
    tmElems <- traverse check elems
    pure $ TmTuple a tmElems

  checkExprRecord a lblExprs = do 
    tmFlds <- traverse checkLabeledExpr lblExprs
    pure $ TmRecord a tmFlds

  checkLabeledExpr (Syntax.Labeled label exp) = do
    let Syntax.Label lbl = label
    case SCU.uncons lbl of
      Nothing -> throwError $ RecordLabelIllFormed ""
      Just { head: ch } 
        | isUppercase ch -> throwError $ RecordLabelIllFormed "Record field name cannot begin with uppercase character."
        | otherwise -> Prop lbl <$> check exp

  checkExprAccess tm = Array.uncons >>> case _ of 
    Nothing -> pure tm
    Just { head:acs, tail:rest } -> 
      case acs of 
        AcsIndex a n -> do
          let 
            tm' = TmField {pos: (termAnn tm).pos ~ a.pos} tm n
          checkExprAccess tm' rest
        AcsProperty a label -> do 
          let 
            Syntax.Label lbl = label
            tm' = TmProperty {pos: (termAnn tm).pos ~ a.pos} tm lbl
          checkExprAccess tm' rest 
    
  checkType = case _ of 
    Syntax.TFree a ident -> case ident of 
      "bool" -> pure $ TBool a
      "nat" -> pure $ TNat a
      "unit" -> pure $ TUnit a
      _ -> throwError $ ExprIllFormed "Atomic types other than nat, bool, unit are not supported."
    Syntax.TFun a argTypes retType -> checkFuncType a (NonEmptyArray.toArray argTypes) retType
    Syntax.TTup a typs -> TTuple a <$> traverse checkType typs
    Syntax.TRecord a lblTyps -> TRecord a <$> 
        traverse 
          (\(Syntax.Labeled (Syntax.Label lbl) it) -> Prop lbl <$> checkType it) 
          lblTyps 
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
  