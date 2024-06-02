module TAPL.TRecon.Syntax.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TAPL.TRecon.Syntax.Position (SourceRange)
import TAPL.TRecon.Syntax.Types (Token, SourceToken, printToken)

type ParseError =
  { err :: ParseErrorType
  , pos :: SourceRange 
  , tokens :: Array SourceToken
  }

data ParseErrorType
  = LexUnexpected 
  | UnexpectedToken Token

derive instance Generic ParseErrorType _ 
instance Show ParseErrorType where
  show = genericShow

prettyPrintError :: ParseError -> String
prettyPrintError { err, pos, tokens } = case err of 
  LexUnexpected -> "Unexpected character"
  UnexpectedToken tok -> "Unexpected token: " <> printToken tok 