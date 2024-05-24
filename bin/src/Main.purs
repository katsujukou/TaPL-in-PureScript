module Main where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Process as Process
import BoolNat.Syntax.WellFormedness as WellFormedness
import TAPL.REPL.Node.Console as Node
import TAPL.REPL.Node.ReadLine as RL
import TAPL.BoolNat.Error (Error(..))
import TAPL.BoolNat.Error as Error
import TAPL.BoolNat.Eval as Eval
import TAPL.BoolNat.Eval.Value (Value)
import TAPL.BoolNat.PrettyPrint as PrettyPrint
import TAPL.BoolNat.Syntax.Parser as Parser
import TAPL.BoolNat.Syntax.Types (Ann, Expr)
import TAPL.BoolNat.TypeCheck as TypeCheck
import TAPL.BoolNat.Types (Type_)

data ReplCommand = Quit | Help | Unknown String

data Input 
  = InpEmpty 
  | InpCmd ReplCommand
  | InpSourceText String 

main :: Effect Unit
main = launchAff_ do
  log "Welcome to TAPL in PureScript!"
  log ""
  log "Enter \x1b[1;34m:q\x1b[0m to exit."
  log "Enter \x1b[1;34m:?\x1b[0m to view usage."
  log ""
  _ <- repl
  liftEffect $ Process.exit 
  where
  prompt = "\x1b[36mTaPLi>\x1b[0m "

  getRawInput :: Aff String
  getRawInput = RL.getRawInput $ Just \line -> do
    prompt <> line 

  repl :: Aff Unit
  repl = do 
    liftEffect $ Node.write prompt
    inp <- getRawInput 
    case parseInput inp of 
      InpEmpty -> repl 
      InpCmd cmd -> case cmd of 
        Quit -> do
          log "\nBye 👋"
          pure unit
        _ -> repl 
      InpSourceText src -> do
        let 
          res = do  
            exp <- parseSourceText src
            tm <- WellFormedness.check exp
            typ <- TypeCheck.infer tm
            val <- Eval.eval tm
            pure { val, typ } 
        case res of 
          Right { val, typ } -> do
            prettyPrint val typ
          Left e -> do
            prettyPrintError src e 
        repl
  
  parseInput :: String -> Input
  parseInput = case _ of 
    "" -> InpEmpty
    inp 
      | Just matched <- Re.match (unsafeRegex """^\s*:([a-z?]+)""" unicode) inp 
      , Just cmd <- NonEmptyArray.last matched -> case cmd of 
        "q" -> InpCmd Quit
        "?" -> InpCmd Help 
        _ -> InpCmd (Unknown cmd)
      | otherwise -> InpSourceText inp

  parseSourceText :: String -> Either Error (Expr Ann)
  parseSourceText = Parser.initialState 
    >>> Parser.runParser Parser.parseExpr 
    >>> fst 
    >>> lmap ParseError

  prettyPrint :: Value -> Type_ -> Aff Unit
  prettyPrint val typ = do
    log $
      "  it = " <> PrettyPrint.printValue val <> "\n" <>
      "     : " <> PrettyPrint.printType typ  <> "\n"

  prettyPrintError :: String -> Error -> Aff Unit 
  prettyPrintError src e = do 
    log "\x1b[31m"
    log $ Error.prettyPrintError src e 
    log "\x1b[0m"

