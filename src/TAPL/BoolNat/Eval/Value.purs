module TAPL.BoolNat.Eval.Value where

import Prelude


data Value 
  = VTrue 
  | VFalse 
  | VZero 
  | VSucc Value

-- derive instance Generic Value _ 
instance Show Value where
  show = case _ of
    VTrue -> "VTrue" 
    VFalse -> "VFalse" 
    VZero -> "VZero"
    VSucc nv -> "(VSucc " <> show nv <> ")"

isNumeric :: Value -> Boolean
isNumeric = case _ of 
  VZero -> true
  VSucc nv -> isNumeric nv
  _ -> false 