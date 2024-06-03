module TAPL.TRecon.REPL where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Console (log, logShow)
import TAPL.TRecon.Error (Error(..))
import TAPL.TRecon.Eval as Eval
import TAPL.TRecon.Eval.Value (Value)
import TAPL.TRecon.PrettyPrint as PP
import TAPL.TRecon.Syntax.Parser (execParser, initialState, parseExpr)
import TAPL.TRecon.Syntax.Types as Syntax
import TAPL.TRecon.TypeCheck as TC
import TAPL.TRecon.Types (Term, Type_, Ann)
import TAPL.TRecon.WellFormedness as WF

parse :: String -> Either Error (Syntax.Expr Syntax.Ann)
parse src = execParser parseExpr (initialState src) # lmap ParseError

check :: Syntax.Expr Syntax.Ann -> Either Error (Term Ann)
check exp = WF.runCheck (WF.check exp)

typecheck :: Term Ann -> Either Error { term :: Term Ann, typ :: Type_ Ann } 
typecheck term = { term, typ: _ } <$> TC.evalTCheck (TC.infer term)
-- typecheckState :: Term Ann -> Either Error { term :: Term Ann, typ :: TC.TCheckState } 
-- typecheckState term = { term, typ: _ } <$> TC.execTCheck (TC.genConstr term)

eval :: forall t. { term :: Term Ann, typ :: t } -> Either Error { value :: Value, typ :: t }
eval { term, typ } = { typ, value: _ } <$> Eval.eval term

repl :: String -> Effect Unit
repl src = src 
  # (parse >=> check >=> typecheck >=> eval)
  # either
    logShow 
    (\{ value, typ } -> log $ " it = " <> PP.printValue value <> "\n    : " <> PP.printType typ)
