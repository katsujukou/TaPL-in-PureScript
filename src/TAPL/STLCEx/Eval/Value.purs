module TAPL.STLCEx.Eval.Value where

import Prelude

import Data.String as Str

data Value 
  = VTrue 
  | VFalse 
  | VZero 
  | VSucc Value
  | VAbs 
  | VUnit
  | VTuple (Array Value)

-- derive instance Generic Value _ 
instance Show Value where
  show = case _ of
    VTrue -> "VTrue" 
    VFalse -> "VFalse" 
    VZero -> "VZero"
    VSucc nv -> "(VSucc " <> show nv <> ")"
    VAbs -> "(VAbs <fun>)"
    VUnit -> "()"
    VTuple vs -> "(VTuple " <> (Str.joinWith ", " $ map show vs) <> ")"

isNumeric :: Value -> Boolean
isNumeric = case _ of 
  VZero -> true
  VSucc nv -> isNumeric nv
  _ -> false 