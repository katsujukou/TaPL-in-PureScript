module TAPL.STLCEx.PrettyPrint where

import Prelude

import Data.Array as Array
import Data.String as String
import TAPL.STLCEx.Eval.Value (Value(..))
import TAPL.STLCEx.Types (Prop(..), Type_(..))

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
      TTuple _ ts
        | Array.length ts == 0 -> "()" 
        | otherwise -> "(" <> (String.joinWith " * " $ map (go true) ts) <> ")"
      TRecord _ flds
        | Array.length flds == 0 -> "{}"
        | otherwise -> "{ " <> String.joinWith ", " (map printProp flds) <> " }"

  printProp (Prop p typ) = p <> " : " <> go false typ

printValue :: Value -> String
printValue = case _ of 
  VTrue -> "true"
  VFalse -> "false"
  VZero -> "0"
  VSucc v -> show $ 1 + realNat 0 v 
  VAbs -> "<fun>"
  VUnit -> "()"
  VTuple vs
    | [] <- vs -> "{}"
    | otherwise -> "{ " <> String.joinWith ", " (printValue <$> vs) <> " }"
  VRecord flds 
    | [] <- flds -> "{}"
    | otherwise -> "{ " <> String.joinWith ", " (printPropValue <$> flds) <> " }"
  where
  printPropValue { prop, value } = prop <> " = " <> printValue value

  realNat n = case _ of 
    VSucc v -> realNat (n + 1) v
    _ -> n
  