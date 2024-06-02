module TAPL.TRecon.Syntax.TokenStream
  ( Stepper
  , TokenStream
  , mkTokenStream
  , pushFront
  , step
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Tuple.Nested (type (/\), (/\))
import TAPL.TRecon.Syntax.Error (ParseError)
import TAPL.TRecon.Syntax.Lexer as L
import TAPL.TRecon.Syntax.Position (emptyPos, (..))
import TAPL.TRecon.Syntax.Types (SourceToken)

newtype TokenStream = TokenStream
  { step :: Stepper
  , buf :: List SourceToken
  }

instance Show TokenStream where
  show _ = "<token stream>"

type Stepper = Unit -> Either ParseError SourceToken /\ TokenStream 

mkTokenStream :: String -> TokenStream
mkTokenStream src = 
  let 
    stepper:: L.LexerState -> Stepper
    stepper s0 _ = 
      let 
        res /\ s1 = L.runLexer L.tokenize s0 
        toks = TokenStream { step: stepper s1, buf: Nil }
      in
        case res of  
          Right a -> Right a /\ toks
          Left err -> Left { err, pos: s0.pos .. s1.pos, tokens: [] } /\ toks

  in 
    TokenStream
      { step: stepper { src, pos: emptyPos }
      , buf: Nil
      }
    
step :: TokenStream -> Either ParseError SourceToken /\ TokenStream
step (TokenStream s) = case s.buf of 
  Nil -> s.step unit
  Cons hd tl -> Right hd /\ TokenStream (s { buf = tl }) 

pushFront :: SourceToken -> TokenStream -> TokenStream 
pushFront tok (TokenStream s) = TokenStream (s { buf = tok:s.buf})