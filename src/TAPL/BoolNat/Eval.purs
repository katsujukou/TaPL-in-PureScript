module TAPL.BoolNat.Eval where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import TAPL.BoolNat.Error (Error(..))
import TAPL.BoolNat.Eval.Value (Value(..), isNumeric)
import TAPL.BoolNat.Types (Term(..), isValue)

{- Evaluation Rule -}
type Result a = Either Error a

throwError :: forall a. Error -> Result a 
throwError = Left

evalSmallStep :: forall a. Term a -> Result (Term a)
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
  TmPred a t
    | TmZero _ <- t -> pure $ TmZero a
    | Just (VSucc nv) <- extractValue t
    , isNumeric nv -> pure t 
    | otherwise -> TmPred a <$> evalSmallStep t
  tm 
    | isValue tm -> pure tm
    | otherwise -> throwError EvalStuck

eval :: forall a. Term a -> Result Value
eval = tailRecM go
  where
  go tm = do
    tm' <- evalSmallStep tm
    case extractValue tm' of
      Just val -> pure $ Done val
      Nothing -> pure $ Loop tm' 
    
extractValue :: forall a. Term a -> Maybe Value
extractValue = case _ of
  TmFalse _ -> Just VFalse
  TmTrue _ -> Just VTrue
  TmZero _ -> Just VZero
  TmSucc _ tm -> VSucc <$> extractValue tm
  _ -> Nothing

