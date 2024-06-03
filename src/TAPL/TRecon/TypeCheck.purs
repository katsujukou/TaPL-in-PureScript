module TAPL.TRecon.TypeCheck where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, get, modify_, put, runStateT)
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Data
import Data.Set as S
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import TAPL.TRecon.Error (Error(..))
import TAPL.TRecon.Types (Ann, Ident, Term(..), Type_(..), Var, emptyAnn, setTypeAnn, termAnn, typEq)

type TypeConstraint a = Tuple (Type_ a) (Type_ a)

type TCheckState = 
  { constrs :: Set (TypeConstraint Ann)
  , next :: Int
  }

data TypeContext
  = TCtxNull 
  | TCtx (Type_ Unit) TypeContext

derive instance Generic TypeContext _ 
instance Show TypeContext where
  show it = genericShow it

initialState :: TCheckState 
initialState = 
  { constrs: Set.empty
  , next: 0
  }

type TCheck a = ReaderT TypeContext (StateT TCheckState (Except Error)) a

runTCheck :: TCheck (Type_ Ann) -> Either Error (Type_ Ann /\ TCheckState)
runTCheck tc = runExcept $ runStateT (runReaderT tc TCtxNull) initialState

evalTCheck :: TCheck (Type_ Ann) -> Either Error (Type_ Ann)
evalTCheck = runTCheck >>> map fst

execTCheck :: TCheck (Type_ Ann) -> Either Error (TCheckState)
execTCheck = runTCheck >>> map snd

withExtendEnv :: forall a b. Type_ a -> TCheck b -> TCheck b 
withExtendEnv typ = local (TCtx (map (const unit) typ))

searchEnv :: Var -> TCheck (Type_ Unit)
searchEnv var = do 
  ctx <- ask
  tailRecM (uncurry go) (var /\ ctx)  
  where
    go = case _, _ of 
      0, TCtx typ _ -> pure $ Done typ
      i, TCtx _ tenv -> pure $ Loop ((i - 1) /\ tenv)
      i, TCtxNull -> throwError $ UnknownIdentifier (show i)

freshTVar :: forall a. a -> TCheck (Type_ a)
freshTVar a = do
  s <- get
  put (s { next = s.next+ 1 })
    $> TVar a ("?x" <> show s.next) 
  
freshTVar_ :: TCheck (Type_ Unit)
freshTVar_ = freshTVar unit 

addConstr :: Type_ Ann -> Type_ Ann -> TCheck Unit
addConstr t1 t2 = do
  modify_ (\s -> s { constrs = Set.insert (t1 /\ t2) s.constrs})

infix 5 addConstr as :=:

genConstr :: Term Ann -> TCheck (Type_ Ann)
genConstr = case _ of 
  TmBound a i -> do
    typ <- searchEnv i
    pure (setTypeAnn a typ)
  TmAbs a mbtyp body
    | Just typ1 <- mbtyp -> do
        typ2 <- withExtendEnv typ1 $ genConstr body
        pure $ TFun a typ1 (setTypeAnn (termAnn body) typ2)
    | otherwise -> do
        typ <- freshTVar emptyAnn
        genConstr $ TmAbs a (Just typ) body 
  TmApp _ tm1 tm2 -> do 
    typ1 <- genConstr tm1
    typ2 <- genConstr tm2
    typx <- freshTVar emptyAnn
    typ1 :=: TFun (termAnn tm1) typ2 typx 
    pure typx
  -- CT-True, CT-False
  TmTrue a -> pure $ TBool a
  TmFalse a -> pure $ TBool a
  -- CT-If
  TmIf _ tm1 tm2 tm3 -> do
    ty1 <- genConstr tm1 
    ty2 <- genConstr tm2 
    ty3 <- genConstr tm3 
    ty1 :=: TBool (termAnn tm1) 
    ty2 :=: ty3 
    pure ty2
  -- CT-Zero
  TmZero a -> pure $ TNat a 
  -- CT-Succ
  TmSucc a tm -> do
    ty <- genConstr tm
    ty :=: TNat a
    pure $ TNat a
  -- CT-Pred
  TmPred a tm -> do
    ty <- genConstr tm
    ty :=: TNat a
    pure $ TNat a
  -- CT-IsZero 
  TmIsZero a tm -> do 
    ty <- genConstr tm
    ty :=: TNat (termAnn tm) 
    pure $ TBool a  
  -- CT-Unit 
  TmUnit a -> pure $ TUnit a
  TmLetIn a mbtyp e1 e2
  --   | Just typ <- mbtyp -> do 

    | otherwise -> do 
        typ <- freshTVar emptyAnn 
        genConstr $ TmLetIn a (Just typ) e1 e2 

type TSubst a = Map Ident (Type_ a)
type TSubstSet a = Array (TSubst a)

emptySigma :: forall a. TSubstSet a
emptySigma = []
 
access :: forall a. TSubstSet a -> Ident -> Maybe (Type_ a)
access sigma id = case Array.uncons sigma of 
  Nothing -> Nothing
  Just {head, tail}
    | Just ty <- Map.lookup id head -> Just ty 
    | otherwise -> access tail id

infix 5 access as !!

single :: forall a. Ident -> Type_ a -> TSubst a
single id typ = Map.singleton id typ

infix 5 single as :->

applyTSubst :: forall a. TSubstSet a -> Type_ a -> Type_ a 
applyTSubst = flip (foldr subst)
  where 
  subst :: TSubst a -> Type_ _ -> Type_ _
  subst sigma = case _ of 
    TVar a ident 
      | Just ty <- Map.lookup ident sigma -> map (const a) ty 
      | otherwise -> TVar a ident
    TFun a ty1 ty2 -> TFun a (subst sigma ty1) (subst sigma ty2)
    ty -> ty
  
substConstr :: forall a. Ord a => TSubstSet a -> Set (TypeConstraint a) -> Set (TypeConstraint a)
substConstr sigma = Set.map (bimap (applyTSubst sigma) (applyTSubst sigma))

freevars :: forall a. Type_ a -> Data.Set Ident 
freevars _ = Set.empty

unconsSet :: forall a. Ord a => Set a -> Maybe (a /\ Set a)
unconsSet s = case Array.uncons (Set.toUnfoldable s) of 
  Nothing -> Nothing
  Just {head, tail} -> Just (head /\ Set.fromFoldable tail)

-- Calculate the principal unifier from currenct constraints set.
unify :: Set (TypeConstraint Ann) -> TCheck (TSubstSet Ann)
unify c = do 
  case unconsSet c of
    Nothing -> pure emptySigma
    Just (eqn /\ c') -> do
      case fst eqn, snd eqn of
        typS, typT
          | typS `typEq` typT -> unify c'
        TVar _ typX, typT 
          | not $ typX `S.member` freevars typT -> 
              unify (substConstr [typX :-> typT] c') <#> (_ <> [typX :-> typT])  
        typS, TVar _ typX
          | not $ typX `S.member` freevars typS -> 
              unify (substConstr [typX :-> typS] c') <#> (_ <> [typX :-> typS])
        TFun _ typS1 typS2, TFun _ typT1 typT2 -> 
          unify (c' `Set.union` (Set.fromFoldable [typS1 /\ typT1, typS2 /\ typT2]))
        ty1, ty2 -> throwError $ UnificationFailed ty1 ty2

infer :: Term Ann -> TCheck (Type_ Ann)
infer tm = do 
  resultTyp <- genConstr tm
  { constrs } <- get
  sigma <- unify constrs
  pure $ applyTSubst sigma resultTyp