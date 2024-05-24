module TAPL.BoolNat.Error where

import Prelude

import Data.Array ((!!))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as Str
import Data.String.Utils as StrUtil
import TAPL.BoolNat.PrettyPrint (printType)
import TAPL.BoolNat.Syntax.Error as PE
import TAPL.BoolNat.Syntax.Position (SourceRange, printRange)
import TAPL.BoolNat.Types (Ident, Type_)


data Error 
  = ParseError PE.ParseError
  | ExprIllFormed String
  | TypeMismatch { pos :: SourceRange, expect :: Type_, found :: Type_ }
  | UnknownIdentifier Ident
  | IllegalApplication
  | EvalStuck

derive instance Generic Error _
instance Show Error where
  show = genericShow

prettyPrintError :: String -> Error -> String
prettyPrintError src = case _ of 
  ParseError e -> "Failed to parse text: \n" 
    <> "  " <> PE.prettyPrintError e
  ExprIllFormed e -> "Expression is ill-formed: \n\n"
    <> "  " <> e
  TypeMismatch {pos, expect, found} -> 
    "Expression is ill-typed:\x1b[0m\n"
      <> srcBlock pos <> "\n"
      <> indicate pos <> "\n\
    \Expect:\n\
    \  " <> printType expect <> "\n\n\
    \Found:\n\
    \  " <> printType found 
  UnknownIdentifier ident -> 
    "Unknown identifier: " <> ident
  IllegalApplication -> 
    "Illegal function application"
  EvalStuck -> "Evaluation stucked."
  where
    srcBlock pos = do
      let 
        lines = Str.split (Pattern "\n") src 
        line  = lines !! (pos.start.ln - 1) 
      case line of 
        Nothing -> ""
        Just ln -> "\x1b[31m" <> show pos.start.ln <> "| \x1b[0m" <> ln

      
    indicate pos 
      | pos.start.ln == pos.end.ln =
          fromMaybe "" do
              indent <- StrUtil.repeat pos.start.col " "
              line <- StrUtil.repeat (pos.end.col - pos.start.col) "^"
              pure $ "  " <> indent <> "\x1b[31m" <> line <> "\x1b[0m"
      | otherwise = ""