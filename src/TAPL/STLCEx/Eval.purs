module TAPL.STLCEx.Eval where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (all, (!!))
import Data.Array as Array
import Data.Either (Either)
import Data.FoldableWithIndex (findWithIndex)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Partial.Unsafe (unsafeCrashWith)
import TAPL.STLCEx.Error (Error(..))
import TAPL.STLCEx.Eval.Value (Value(..), isNumeric)
import TAPL.STLCEx.Types (Ann, Prop(..), Term(..), Var, propKey, propValue)

type Eval a = Either Error a

isValue :: forall a. Term a -> Boolean
isValue = case _ of 
  TmFalse _ -> true
  TmTrue _ -> true
  TmZero _ -> true
  TmSucc _ tm -> isValue tm
  TmAbs _ _ _ -> true
  TmUnit _ -> true
  TmTuple _ tms -> all isValue tms
  TmRecord _ flds -> all (propValue >>> isValue) flds
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
  TmLetIn a t1 t2 
    | isValue t1 -> pure $ shift0 (-1) $ (subst 0 (shift0 1 t1) t2)
    | otherwise -> TmLetIn a <$> evalSmallStep t1 <*> pure t2
  tmTpl@(TmTuple a ts)
    | Just v <- findWithIndex (\_ t -> not (isValue t)) ts -> do 
        tm' <- evalSmallStep v.value
        case Array.modifyAt v.index (const tm') ts of
          Just ts' -> pure $ TmTuple a ts' 
          _ -> unsafeCrashWith "Impossible"
    | otherwise -> do
      pure tmTpl
  tmRcd@(TmRecord a tmFlds) 
    | Just { index, value: tmFld } <- findWithIndex 
      (\_ tm -> not $ isValue $ propValue tm) 
      tmFlds -> do 
        tmFld' <- traverse evalSmallStep tmFld
        case Array.modifyAt index (const tmFld') tmFlds of
          Just tmFlds' -> pure $ TmRecord a tmFlds' 
          _ -> unsafeCrashWith "Impossible"
    | otherwise -> do
      pure tmRcd
  TmField a tm n
    | not (isValue tm) -> do 
        tm' <- evalSmallStep tm
        pure $ TmField a tm' n 
    | otherwise -> case tm of
        TmTuple _ ts
          | Just t <- ts !! n -> pure t 
        _ -> throwError $ EvalStuck
  TmProperty a tm prop 
    | not (isValue tm) -> do 
        tm' <- evalSmallStep tm 
        pure $ TmProperty a tm' prop 
    | otherwise -> case tm of 
        TmRecord _ flds 
          | Just (Prop _ t) <- Array.find ((_ == prop) <<< propKey) flds -> pure t
        _ -> throwError $ EvalStuck
  tmfix@(TmFix a tmFn)
    -- E-FIXBETA
    | TmAbs _ _ t2 <- tmFn -> pure $ shift0 (-1) $ subst 0 (shift0 1 tmfix) t2
    -- E-FIX
    | otherwise -> TmFix a <$> evalSmallStep tmFn
  tm
    | isValue tm -> pure tm
    | otherwise -> throwError $ EvalStuck

subst :: forall a. Show a => Var -> Term a -> Term a -> Term a
subst i t0 = case _ of
  t1@(TmBound _ j)
    | i == j -> t0 
    | otherwise -> t1
  TmApp a t1 t2 -> TmApp a (subst i t0 t1) (subst i t0 t2) 
  TmIf a t1 t2 t3 -> TmIf a (subst i t0 t1) (subst i t0 t2) (subst i t0 t3)
  TmIsZero a t -> TmIsZero a $ subst i t0 t 
  TmSucc a t -> TmSucc a (subst i t0 t)
  TmPred a t -> TmPred a $ subst i t0 t
  TmAbs a typ t -> TmAbs a typ (subst (i + 1) (shift0 1 t0) t)
  TmLetIn a t1 t2 -> TmLetIn a (subst i t0 t1) (subst (i + 1) (shift0 1 t0) t2)
  TmTuple a ts -> TmTuple a (map (subst i t0) ts)
  TmRecord a flds -> TmRecord a (map (subst i t0) <$> flds)
  TmField a t n -> TmField a (subst i t0 t) n
  TmProperty a t p -> TmProperty a (subst i t0 t) p
  TmFix a t -> TmFix a (subst i t0 t)
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
  TmSucc a t -> TmSucc a (shift d c t)
  TmIsZero a t -> TmIsZero a (shift d c t)
  TmTuple a ts -> TmTuple a (shift d c <$> ts)
  TmRecord a flds -> TmRecord a (map (shift d c) <$> flds )
  TmField a t n -> TmField a (shift d c t) n
  TmProperty a t p -> TmProperty a (shift d c t) p
  TmFix a t -> TmFix a (shift d c t)
  t -> t

extractValue :: forall a. Term a -> Maybe Value 
extractValue = case _ of 
  TmTrue _ -> Just VTrue
  TmFalse _ -> Just VFalse 
  TmZero _ -> Just VZero
  TmSucc _ t -> VSucc <$> extractValue t
  TmAbs _ _ _ -> Just $ VAbs
  TmUnit _ -> Just VUnit 
  TmTuple _ tms -> VTuple <$> traverse extractValue tms
  TmRecord _ flds -> VRecord <$> traverse (\(Prop prop v) -> ({ prop, value: _ } <$> extractValue v)) flds
  _ -> Nothing
