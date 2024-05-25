module TAPL.STLC.Eval where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import TAPL.STLC.Error (Error(..))
import TAPL.STLC.Eval.Value (Value(..), isNumeric)
import TAPL.STLC.Types (Ann, Term(..), Var)

type Eval a = Either Error a

isValue :: forall a. Term a -> Boolean
isValue = case _ of 
  TmFalse _ -> true
  TmTrue _ -> true
  TmZero _ -> true
  TmSucc _ tm -> isValue tm
  TmAbs _ _ _ -> true
  _ -> false
   
eval :: Term Ann -> Eval Value
eval = tailRecM go
  where
  go tm = do
    tm' <- evalSmallStep tm
    case extractValue tm' of
      Just val -> pure $ Done val
      Nothing -> pure $ Loop tm'  

evalSmallStep :: Term Ann -> Eval (Term Ann)
evalSmallStep = case _ of 
  -- E-IfTrue & E-IfFalse
  TmIf _ (TmTrue _) t2 _ -> pure t2  
  TmIf _ (TmFalse _) _ t3 -> pure t3 
  -- E-If
  TmIf a t1 t2 t3 -> TmIf a <$> evalSmallStep t1 <*> pure t2 <*> pure t3
  -- E-Succ 
  TmSucc a t -> TmSucc a <$> (evalSmallStep t)
  TmIsZero a t 
    | TmZero _ <- t -> pure $ TmTrue a
    | Just (VSucc nv) <- extractValue t
    , isNumeric nv -> pure $ TmFalse a 
    | otherwise -> TmIsZero a <$> evalSmallStep t
  TmPred a t -> case t of 
      TmZero a' -> pure $ TmZero a' 
      TmSucc _ t1
        | isValue t1 -> pure t1
        | otherwise -> throwError EvalStuck
      _ -> TmPred a <$> evalSmallStep t
  TmApp _ (TmAbs _ _ body) t2 
    | isValue t2 -> pure $ shift0 (-1) $ (subst 0 (shift0 1 t2) body)
  TmApp a t1 t2
    | isValue t1 -> TmApp a t1 <$> evalSmallStep t2
    | otherwise -> do 
        t1' <- evalSmallStep t1
        pure $ TmApp a t1' t2 
  tm
    | isValue tm -> pure tm
    | otherwise -> throwError $ EvalStuck

subst :: forall a. Var -> Term a -> Term a -> Term a
subst i t0 = case _ of
  t1@(TmBound _ j)
    | i == j -> t0 
    | otherwise -> t1
  TmApp a t1 t2 -> TmApp a (subst i t0 t1) (subst i t0 t2) 
  TmIf a t1 t2 t3 -> TmIf a (subst i t0 t1) (subst i t0 t2) (subst i t0 t3)
  TmIsZero a t -> TmIsZero a $ subst i t0 t 
  TmPred a t -> TmPred a $ subst i t0 t
  TmAbs a typ t -> TmAbs a typ (subst (i + 1) (shift 1 0 t0) t)
  t2 -> t2 

shift0 :: forall a. Int -> Term a -> Term a 
shift0 d = shift d 0

shift :: forall a. Int -> Int -> Term a -> Term a 
shift d c = case _ of 
  TmBound a j 
    | j < c -> TmBound a j 
    | otherwise -> TmBound a (j + d)
  TmApp a t1 t2 -> TmApp a (shift d c t1) (shift d c t2)
  TmAbs a typ body -> TmAbs a typ (shift d (c + 1) body)
  TmIf a t1 t2 t3 -> TmIf a (shift d c t1) (shift d c t2) (shift d c t3)
  TmPred a t -> TmPred a (shift d c t)
  TmIsZero a t -> TmIsZero a (shift d c t)
  t -> t

extractValue :: forall a. Term a -> Maybe Value 
extractValue = case _ of 
  TmTrue _ -> Just VTrue
  TmFalse _ -> Just VFalse 
  TmZero _ -> Just VZero
  TmSucc _ t -> VSucc <$> extractValue t
  TmAbs _ _ _ -> Just $ VAbs 
  _ -> Nothing