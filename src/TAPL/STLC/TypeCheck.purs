module TAPL.STLC.TypeCheck where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Data.Either (Either)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import TAPL.STLC.Error (Error(..))
import TAPL.STLC.Types (Ann, Term(..), Type_(..), Var, termAnn)

data TEnv
  = TENull 
  | TEnv (Type_ Unit) TEnv

emptyEnv :: TEnv 
emptyEnv = TENull

type TCheck a = StateT TEnv (Except Error) a

runTCheck :: TCheck (Type_ Unit) -> Either Error (Type_ Unit)
runTCheck tc = runExcept $ evalStateT tc TENull
extendEnv :: Type_ Unit -> TCheck Unit 
extendEnv typ = modify_ (TEnv typ)

searchEnv :: Var -> TCheck (Type_ Unit)
searchEnv var = do 
  tenv <- get
  tailRecM (uncurry go) (var /\ tenv)  
  where
    go = case _, _ of 
      0, TEnv typ _ -> pure $ Done typ
      i, TEnv _ tenv -> pure $ Loop ((i - 1) /\ tenv)
      i, TENull -> throwError $ UnknownIdentifier (show i)

infer :: Term Ann -> TCheck (Type_ Unit)
infer = case _ of
  -- T-True, T-False
  TmTrue _ -> pure (TBool unit)
  TmFalse _ -> pure (TBool unit)
  -- T-If
  TmIf _ tm1 tm2 tm3 -> do
    check tm1 (TBool unit)
    t1 <- infer tm2 
    t2 <- infer tm3 
    if t1 == t2 then pure t1 
    else throwError $ TypeMismatch { pos: (termAnn tm3).pos, expect: t1, found: t2 }
  -- T-Zero
  TmZero _ -> pure $ TNat unit
  -- T-Succ
  TmSucc _ tm -> check tm (TNat unit) $> (TNat unit)
  -- T-Pred
  TmPred _ tm -> check tm (TNat unit) $> (TNat unit)
  -- T-IsZero 
  TmIsZero _ tm -> do 
    check tm (TNat unit) $> (TBool unit)
  TmBound _ i -> do 
    typ <- searchEnv i
    pure $ map (const unit) typ
  TmApp _ tm1 tm2 -> infer tm1 >>= case _ of 
    TFun _ t11 t12 -> check tm2 t11 *> pure t12
    _ -> throwError IllegalApplication
  TmAbs _ typ body -> do 
    let typ1 = map (const unit) typ 
    typ2 <- extendEnv typ1
      *> infer body
    pure $ TFun unit typ1 typ2

check :: Term Ann -> Type_ Unit -> TCheck Unit
check tm t2 = do
  t1 <- infer tm
  unless (map (const unit) t1 == t2) do
    throwError (TypeMismatch {pos: (termAnn tm).pos, expect: t2, found: map (const unit) t1 })
