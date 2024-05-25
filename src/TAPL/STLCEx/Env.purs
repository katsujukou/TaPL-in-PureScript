module TAPL.STLCEx.Env where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import TAPL.STLCEx.Types (Ident, Type_, Var)

type Occurrence = List Int

data VarEnv 
  = VEnvNull 
  | VEnv (Map Ident Occurrence) VarEnv 

derive instance Eq VarEnv 
derive instance Generic VarEnv _ 
instance Show VarEnv where
  show it = genericShow it 

extendVarEnvNew :: VarEnv -> Ident -> Occurrence -> VarEnv
extendVarEnvNew env s o = VEnv (Map.singleton s o) env 

searchVarEnv :: Ident -> VarEnv -> Maybe (Var /\ Occurrence)
searchVarEnv s = go 0 
  where 
  go i = case _ of 
    VEnvNull -> Nothing
    VEnv idents env 
      | Just o <- Map.lookup s idents -> Just (i /\ o)
      | otherwise -> go (i + 1) env 

type TypeEnv = List (Type_ Unit) 

extendTypeEnv :: forall a. Type_ a -> TypeEnv -> TypeEnv 
extendTypeEnv typ = List.Cons (map (const unit) typ)

searchTypeEnv :: Var -> TypeEnv -> Maybe (Type_ Unit)
searchTypeEnv = flip List.(!!)
