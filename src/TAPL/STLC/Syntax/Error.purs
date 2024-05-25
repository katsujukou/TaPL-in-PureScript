module TAPL.STLC.Syntax.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TAPL.STLC.Syntax.Types (Token, printToken)

data ParseError
  = LexUnexpected 
  | UnexpectedToken Token

derive instance Generic ParseError _ 
instance Show ParseError where
  show = genericShow

prettyPrintError :: ParseError -> String
prettyPrintError = case _ of 
  LexUnexpected -> "Unexpected character"
  UnexpectedToken tok -> "Unexpected token: " <> printToken tok