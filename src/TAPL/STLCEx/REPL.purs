module TAPL.STLCEx.REPL where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Console (log, logShow)
import TAPL.STLCEx.Error (Error(..))
import TAPL.STLCEx.Eval as Eval
import TAPL.STLCEx.Eval.Value (Value)
import TAPL.STLCEx.PrettyPrint as PP
import TAPL.STLCEx.Syntax.Parser (execParser, initialState, parseExpr)
import TAPL.STLCEx.Syntax.Types as Syntax
import TAPL.STLCEx.TypeCheck as TC
import TAPL.STLCEx.Types (Ann, Term, Type_)
import TAPL.STLCEx.WellFormedness as WF

parse :: String -> Either Error (Syntax.Expr Syntax.Ann)
parse src = execParser parseExpr (initialState src) # lmap ParseError

check :: Syntax.Expr Syntax.Ann -> Either Error (Term Ann)
check exp = WF.runCheck (WF.check exp)

typecheck :: Term Ann -> Either Error { term :: Term Ann, typ :: Type_ Unit } 
typecheck term = { term, typ: _ } <$> TC.runTCheck (TC.infer term)

eval :: { term :: Term Ann, typ :: Type_ Unit } -> Either Error { value :: Value, typ :: Type_ Unit }
eval { term, typ } = { typ, value: _ } <$> Eval.eval term

repl :: String -> Effect Unit
repl src = src 
  # (parse >=> check >=> typecheck >=> eval)
  # either
    logShow 
    (\{ value, typ } -> log $ " it = " <> PP.printValue value <> "\n    : " <> PP.printType typ)
