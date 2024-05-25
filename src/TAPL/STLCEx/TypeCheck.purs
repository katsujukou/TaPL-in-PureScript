module TAPL.STLCEx.TypeCheck where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Either (Either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import TAPL.STLCEx.Env as Env
import TAPL.STLCEx.Error (Error(..))
import TAPL.STLCEx.Types (Ann, Term(..), Type_(..), Var, termAnn)

type TCheck a = ReaderT Env.TypeEnv (Except Error) a

runTCheck :: TCheck (Type_ Unit) -> Either Error (Type_ Unit)
runTCheck tc = runExcept $ runReaderT tc Nil

withExtendEnv :: forall a b. Type_ a -> TCheck b -> TCheck b 
withExtendEnv typ m = local (Env.extendTypeEnv (map (const unit)typ)) m

searchEnv :: Var -> TCheck (Type_ Unit)
searchEnv var = do 
  tenv <- ask
  case Env.searchTypeEnv var tenv of 
    Just typ -> pure typ 
    _ -> throwError $ UnknownIdentifier (show var)

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
  TmAbs _ typ1 body -> do 
    typ2 <- withExtendEnv typ1 (infer body)
    pure $ TFun unit (map (const unit) typ1) typ2
  TmUnit _ -> pure $ TUnit unit 
  TmLetIn _ tm1 tm2 -> do
    typ1 <- infer tm1
    withExtendEnv typ1 (infer tm2) 
-- fun (x:nat) (y:bool) -> let f = fun (x:nat) -> pred x in if y then x else f x

check :: Term Ann -> Type_ Unit -> TCheck Unit
check tm t2 = do
  t1 <- infer tm
  unless (map (const unit) t1 == t2) do
    env <- ask
    let _ = unsafePerformEffect (logShow env)
    throwError (TypeMismatch {pos: (termAnn tm).pos, expect: t2, found: map (const unit) t1 })
