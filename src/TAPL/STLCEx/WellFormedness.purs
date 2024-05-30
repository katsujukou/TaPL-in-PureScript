module TAPL.STLCEx.WellFormedness where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, modify)
import Data.Array (elem, foldl, foldr)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import TAPL.BoolNat.Syntax.Position ((~))
import TAPL.STLCEx.Env (Occurrence, VarEnv(..))
import TAPL.STLCEx.Env as Env
import TAPL.STLCEx.Error (Error(..))
import TAPL.STLCEx.Syntax.Types (Accessor(..), Const(..), Expr(..), Pattern(..), labeledLabel)
import TAPL.STLCEx.Syntax.Types as Syntax
import TAPL.STLCEx.Syntax.Utils (isUppercase)
import TAPL.STLCEx.Types (Ann, Prop(..), Term(..), Type_(..), Ident, emptyAnn, termAnn, typeAnn)

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
    "fix" -> throwError $ ExprIllFormed "fix in unexpected position"
    _ -> do 
      env <- ask
      case Env.searchVarEnv ident env of 
        Just (i /\ _) -> pure $ TmBound a i 
        Nothing -> throwError $ (UnknownIdentifier ident)
  ExprApp _ expAbs expArgs
    | [arg] <- NonEmptyArray.toArray expArgs
    , ExprIdent a ident <- expAbs -> case ident of 
        "succ" -> do 
          tmArg <- check arg 
          pure $ TmSucc { pos: a.pos ~ (termAnn tmArg).pos } tmArg
        "pred" -> do
          tmArg <- check arg
          pure $ TmPred { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "isZero" -> do
          tmArg <- check arg
          pure $ TmIsZero { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
        "fix" -> do
          tmArg <- check arg 
          pure $ TmFix { pos: a.pos ~ (termAnn tmArg).pos } tmArg 
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
  ExprLetRec a binders e -> do 
    desugared <- desugarLetrecBinders a (NonEmptyArray.toArray binders) e
    check desugared
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
  -- _ -> throwError $ ExprIllFormed "not implemented"
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
  
  desugarLetrecBinders a binders e = do 
    rcdVar <- freshVar
    -- The record containing 
    recBindGroup <- binders 
        # traverse \({ binder: Syntax.Binder _ pat typ, expr }) -> 
            case pat of 
              Syntax.PatVar _ ident -> case typ of
                Nothing -> throwError $ BinderNotAnnotated { pos: a.pos } 
                Just t -> 
                  let 
                    label :: forall a. a -> Syntax.Labeled a
                    label = Syntax.Labeled (Syntax.Label ident) 
                  in 
                    pure $ (label t) /\ (label $ expr)
              _ -> throwError $ ExprIllFormed "You cannot pattern destructuring binders in let rec binding group"
    let 
      bindGrpTyps = fst <$> recBindGroup
      recBindGrpTyps = Syntax.TRecord emptyAnn bindGrpTyps
      recBindGrpRcd' = replaceRecursiveCall rcdVar (labeledLabel <$> bindGrpTyps) 
        $ Syntax.ExprRecord emptyAnn (snd <$> recBindGroup)

      recFn = Syntax.ExprAbs
        emptyAnn 
        (NonEmptyArray.singleton $
          Syntax.Binder emptyAnn (Syntax.PatVar emptyAnn rcdVar) (Just recBindGrpTyps))
        recBindGrpRcd'

      fixed = Syntax.ExprApp 
        emptyAnn 
        (Syntax.ExprIdent emptyAnn "fix")
        (NonEmptyArray.singleton recFn)
    fixedVar <- freshVar 
    pure $ 
      Syntax.ExprLet
        emptyAnn
        (Syntax.Binder emptyAnn (Syntax.PatVar emptyAnn fixedVar) (Just recBindGrpTyps))
        fixed
        (wrapAuxLet fixedVar bindGrpTyps e)
    where
      replaceRecursiveCall :: Ident -> Array Syntax.Label -> Syntax.Expr _ -> Syntax.Expr _ 
      replaceRecursiveCall rcdVar lbls exp = do
        let 
          rcdExp = Syntax.ExprIdent emptyAnn rcdVar
          access e1 prop = Syntax.ExprAccess 
            emptyAnn 
            e1 
            (NonEmptyArray.singleton $ Syntax.AcsProperty emptyAnn (Syntax.Label prop))
        foldr
          (\(Syntax.Label lbl) exp' -> exprSubst lbl (rcdExp `access` lbl) exp')
          exp
          lbls

      wrapAuxLet :: Ident -> Array (Syntax.Labeled (Syntax.Type_ _)) -> Syntax.Expr _ -> Syntax.Expr _
      wrapAuxLet rcdVar lblTyps expr = foldr
        ( \(Syntax.Labeled label@(Syntax.Label lbl) typ) exp -> 
            Syntax.ExprLet 
              emptyAnn
              (Syntax.Binder emptyAnn (Syntax.PatVar emptyAnn lbl) (Just typ))
              (Syntax.ExprAccess 
                emptyAnn 
                (Syntax.ExprIdent emptyAnn rcdVar) 
                (NonEmptyArray.singleton (Syntax.AcsProperty emptyAnn label)))
              exp
        )
        expr
        lblTyps
      
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
  
exprSubst :: forall a. Ident -> Syntax.Expr a -> Syntax.Expr a -> Syntax.Expr a
exprSubst var e1 = case _ of
  e2@(Syntax.ExprConst _ _) -> e2 
  e2@(Syntax.ExprAbs a args body) ->
    let
      boundvars = (collectPatVars <<< Syntax.binderPattern) =<< NonEmptyArray.toArray args
    in 
      if var `elem` boundvars then e2
      else do
        let fv = freevars e1 
        if Array.null (Array.intersect fv boundvars) then
          Syntax.ExprAbs a args (exprSubst var e1 body)
        else 
          unsafeCrashWith "TODO: variable captured!" 
  Syntax.ExprAccess a exp path -> Syntax.ExprAccess a (exprSubst var e1 exp) path
  Syntax.ExprApp a abs args -> Syntax.ExprApp a (exprSubst var e1 abs) (exprSubst var e1 <$> args)
  Syntax.ExprAscription a exp typ -> Syntax.ExprAscription a (exprSubst var e1 exp) typ
  e2@(Syntax.ExprIdent _ id) 
    | id == var -> e1 
    | otherwise -> e2
  Syntax.ExprIf a exp1 exp2 exp3 -> Syntax.ExprIf a (exprSubst var e1 exp1) (exprSubst var e1 exp2) (exprSubst var e1 exp3)
  e2@(Syntax.ExprLet a binder exp1 exp2) -> do
    let bv = collectPatVars $ Syntax.binderPattern binder
    if var `elem` bv then e2 
    else do
      let fv = freevars e1 
      if (Array.null $ Array.intersect fv bv) then 
        Syntax.ExprLet a binder (exprSubst var e1 exp1) (exprSubst var e1 exp2)
      else 
        unsafeCrashWith "TODO: variable captured!" 
  -- Syntax.ExprLetRec 
  Syntax.ExprRecord a props -> Syntax.ExprRecord a (map (exprSubst var e1) <$> props)
  Syntax.ExprSucc a exp -> Syntax.ExprSucc a (exprSubst var e1 exp)
  Syntax.ExprTuple a exps -> Syntax.ExprTuple a (exprSubst var e1 <$> exps) 
  e2 -> e2

freevars :: forall a. Syntax.Expr a -> Array Ident 
freevars = case _ of
  Syntax.ExprAbs _ args body ->
    let 
      boundvars = ((collectPatVars <<< Syntax.binderPattern) =<< NonEmptyArray.toArray args) 
    in 
      Array.filter (not <<< (_ `Array.elem` boundvars)) $ freevars body 
  Syntax.ExprAccess _ exp _ -> freevars exp
  Syntax.ExprApp _ abs args -> freevars abs <> (freevars =<< NonEmptyArray.toArray args)
  Syntax.ExprAscription _ exp _ -> freevars exp
  Syntax.ExprIdent _ id -> [id]
  Syntax.ExprIf _ e1 e2 e3 -> Array.foldMap freevars [e1, e2, e3] 
  Syntax.ExprLet _ b e1 e2 -> do 
    let boundvars = collectPatVars $ Syntax.binderPattern b 
    freevars e1 <> (Array.filter (not <<< (_ `Array.elem` boundvars)) $ freevars e2) 
  Syntax.ExprLetRec _ bdrs exp -> do 
    let binders = NonEmptyArray.toArray bdrs
    let boundvars = Array.foldMap (collectPatVars <<< Syntax.binderPattern <<< _.binder) binders 
    Array.filter (not <<< (_ `Array.elem` boundvars)) 
      $ (Array.nub $ (Array.foldMap (freevars <<< _.expr) binders) <> freevars exp)
  Syntax.ExprRecord _ props -> Array.foldMap (freevars <<< Syntax.labeledValue) props
  Syntax.ExprSucc _ e -> freevars e
  Syntax.ExprTuple _ exps -> Array.foldMap freevars exps
  _ -> []
collectPatVars :: forall a. Syntax.Pattern a -> Array Ident
collectPatVars = case _ of 
  Syntax.PatVar _ var -> [var]
  Syntax.PatWildcard _ -> []