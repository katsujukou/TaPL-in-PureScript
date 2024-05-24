module TAPL.REPL.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TAPL.BoolNat.Error as BoolNat
import TAPL.BoolNat.Syntax.Error as BoolNatP

data ReplError
  = ParseError BoolNatP.ParseError
  | WellFormednessError BoolNat.Error
  | TypeError BoolNat.Error
  | EvalError BoolNat.Error

-- derive instance Eq Error 
derive instance Generic ReplError _
instance Show ReplError where
  show = genericShow 
