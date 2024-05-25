module TAPL.STLCEx.PrettyPrint where

import Prelude

import TAPL.STLCEx.Eval.Value (Value(..))
import TAPL.STLCEx.Types (Type_(..))

printType :: forall a. Type_ a -> String
printType = go false 
  where 
  go neg = 
    case _ of 
      TBool _ -> "bool"
      TNat _ -> "nat"
      TFun _ t1 t2 
        | neg -> "(" <> go true t1 <> " -> " <> go false t2 <> ")"
        | otherwise -> go true t1 <> " -> " <> go false t2
      TUnit _ -> "unit"
    
printValue :: Value -> String
printValue = case _ of 
  VTrue -> "true"
  VFalse -> "false"
  VZero -> "0"
  VSucc v -> show $ 1 + realNat 0 v 
  VAbs -> "<fun>"
  VUnit -> "()"
  where
  realNat n = case _ of 
    VSucc v -> realNat (n + 1) v
    _ -> n
  