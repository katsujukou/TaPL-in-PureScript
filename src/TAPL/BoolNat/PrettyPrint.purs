module TAPL.BoolNat.PrettyPrint where

import Prelude

import TAPL.BoolNat.Eval.Value (Value(..))
import TAPL.BoolNat.Types (Type_(..))

printValue :: Value -> String
printValue = case _ of 
  VTrue -> "true"
  VFalse -> "false"
  VZero -> "0"
  VSucc v -> show $ 1 + realNat 0 v 
  where
  realNat n = case _ of 
    VSucc v -> realNat (n + 1) v
    _ -> n
  
printType :: Type_ -> String
printType = case _ of 
  TBool -> "bool"
  TNat -> "nat"
  TFun t1 t2 -> printType t1 <> "->" <> printType t2  