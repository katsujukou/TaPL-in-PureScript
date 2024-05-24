module TAPL.BoolNat.TypeCheck where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import TAPL.BoolNat.Error (Error(..))
import TAPL.BoolNat.Result (Result, throwError)
import TAPL.BoolNat.Syntax.Types (Ann)
import TAPL.BoolNat.Types (Name, Term(..), Type_(..), termAnn)

type TEnv = Map Name Type_

emptyEnv :: TEnv 
emptyEnv = Map.empty

infer :: Term Ann -> Result Type_
infer = case _ of
  -- T-True, T-False
  TmTrue _ -> pure TBool
  TmFalse _ -> pure TBool
  -- T-If
  TmIf a tm1 tm2 tm3 -> do
    check tm1 TBool
    t1 <- infer tm2 
    t2 <- infer tm3 
    if t1 == t2 then pure t1 
    else throwError $ TypeMismatch { pos: (termAnn tm3).pos, expect: t1, found: t2 }
  -- T-Zero
  TmZero _ -> pure TNat
  -- T-Succ
  TmSucc _ tm -> check tm TNat $> TNat
  -- T-Pred
  TmPred _ tm -> check tm TNat $> TNat
  -- T-IsZero 
  TmIsZero _ tm -> do 
    check tm TNat $> TBool
 
check :: Term Ann -> Type_ -> Result Unit
check tm t2 = do
  t1 <- infer tm
  unless (t1 == t2) do
    throwError (TypeMismatch {pos: (termAnn tm).pos, expect: t2, found: t1 })