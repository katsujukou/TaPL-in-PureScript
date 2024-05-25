module TAPL.STLC.Syntax.Lexer where 

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import TAPL.STLC.Syntax.Error (ParseError(..))
import TAPL.STLC.Syntax.Position (SourcePhrase, SourcePos, advancePos, charDelta, mapPhrase, stringDelta, (..), (@@))
import TAPL.STLC.Syntax.Types (Keyword(..), SourceToken, Token(..))

type LexerState = { src :: String, pos :: SourcePos }

type LexResult a = Either ParseError a  

newtype Lexer a = Lexer (LexerState -> LexResult a /\ LexerState)

instance Functor Lexer where
  map f (Lexer k) = Lexer \s0 -> lmap (map f) (k s0)

instance Apply Lexer where 
  apply (Lexer k1) (Lexer k2) = Lexer \s0 -> 
    case k1 s0 of 
      Left e /\ s1 -> Left e /\ s1 
      Right f /\ s1 -> lmap (map f) (k2 s1)

instance Applicative Lexer where
  pure a = Lexer \s0 -> Right a /\ s0 

instance Bind Lexer where
  bind (Lexer k1) f = Lexer \s0 -> 
    case k1 s0 of 
      Left e /\ s1 -> Left e /\ s1
      Right a /\ s1 -> let Lexer k2 = f a in k2 s1

instance Monad Lexer 

instance Alt Lexer where 
  alt (Lexer k1) (Lexer k2) = Lexer \s0 ->
    case k1 s0 of
      Right a /\ s1 -> Right a /\ s1
      Left err /\ s1 
        | s1 == s0 -> k2 s1 
        | otherwise -> Left err /\ s1 

runLexer :: forall a. Lexer a -> LexerState -> LexResult a /\ LexerState 
runLexer (Lexer k) s0 = k s0

failwith :: forall a. ParseError -> Lexer a
failwith err = Lexer \s0 -> Left err /\ s0 

getCurrentState :: Lexer LexerState
getCurrentState = Lexer \s0 -> Right s0 /\ s0

setState :: LexerState -> Lexer Unit 
setState s0 = Lexer \_ -> Right unit /\ s0

type SourceString = SourcePhrase String

tokenize :: Lexer SourceToken
tokenize = do
  _ <- whitespace
  (punctuation 
  <|> nat 
  <|> ident 
  <|> operator
  <|> eof
  )
--   (punctuation 
--   <|> operator 
--   <|> ident 
--   <|> eof)

whitespace :: Lexer Unit 
whitespace = void $ 
  many
    ( char '\n' 
    <|> char '\r' 
    <|> char ' ' 
    <|> char '\t'
    )

eof :: Lexer SourceToken 
eof = Lexer \s0 -> 
  if s0.src == "" then Right (TokEOF @@ (s0.pos .. s0.pos)) /\ s0
  else Left LexUnexpected /\ s0

punctuation :: Lexer SourceToken 
punctuation = do
  s0 <- getCurrentState
  ch <- anychar
  case ch.it of
    '(' -> pure $ mapPhrase (const TokLeftParens) ch
    ')' -> pure $ mapPhrase (const TokRightParens) ch
    _ -> setState s0 *> failwith (LexUnexpected)

operator :: Lexer SourceToken
operator = do
  matched <- regex operatorRegex
  case matched.it of 
    "=" -> pure $ matched {it = TokEqual } 
    ":" -> pure $ matched {it = TokColon }
    "->" -> pure $ matched {it = TokRightArrow}
    op -> pure $ matched {it = TokOperator op}

  where
    operatorRegex = """[!#$%&=\-\^~|\\@+*:?/<>]+"""
nat :: Lexer SourceToken
nat = do
  matched <- regex """[0-9]+"""
  case Int.fromString matched.it of 
    Nothing -> unsafeCrashWith "Impossible"
    Just n -> pure $ matched {it = TokNat n}
-- operator = do
--   -- s0 <- getCurrentState 
--   matched <- regex """[!#$5&=\-^~|@*:+<>.?/\\]+"""
--   case matched.it of
--     "." -> pure $ mapPhrase (const TokDot) matched 
--     "=" -> pure $ mapPhrase (const TokEqual) matched 
--     "*" -> pure $ mapPhrase (const TokStar) matched
--     ":" -> pure $ mapPhrase (const TokColon) matched
--     "->" -> pure $ mapPhrase (const TokRightArrow) matched
--     _ -> pure $ mapPhrase TokOperator matched 
  
ident :: Lexer SourceToken 
ident = do
  matched <- regex """[a-zA-Z0-9][a-zA-Z0-9'_]*"""
  case parseKeyword matched.it of 
    Just kw -> pure $ matched {it = TokReserved kw}
    _ -> case matched.it of 
      "true" -> pure $ mapPhrase (const (TokBool true)) matched 
      "false" -> pure $ mapPhrase (const (TokBool false)) matched 
      _ -> pure $ mapPhrase TokIdent matched

anychar :: Lexer (SourcePhrase Char)
anychar = Lexer \s0@{pos, src} -> case SCU.uncons src of
  Nothing -> Left (LexUnexpected) /\ (s0 { src = "" })
  Just {head, tail} -> 
    let 
      pos' = advancePos (charDelta head) pos 
    in 
      Right (head @@ (pos .. pos')) /\ { src: tail, pos: pos' }

char :: Char -> Lexer (SourcePhrase Char) 
char ch = Lexer \s0 -> case SCU.uncons s0.src of
   Nothing -> Left (LexUnexpected) /\ (s0 {src = ""})
   Just { head, tail }
     | head == ch -> 
         let 
           pos' = advancePos (charDelta head) s0.pos
         in 
           Right (ch @@ (s0.pos .. pos')) /\ {src: tail, pos: pos'} 
     | otherwise -> Left (LexUnexpected) /\ s0

many :: forall a. Lexer a -> Lexer (Array a)
many (Lexer k) = Lexer \s0 -> go [] s0
  where 
  go ls s = case k s of
    Right a /\ s' -> go (ls <> [a]) s'
    Left e /\ s' 
      | s == s' -> Right ls /\ s 
      | otherwise -> Left e /\ s'

regex :: String -> Lexer (SourcePhrase String)
regex regexStr = Lexer \s0 -> 
  case Re.match matchRegex s0.src of
    Just groups
      | Just match <- NonEmptyArray.head groups ->
          let
            matchLength = SCU.length match 
            pos' = advancePos (stringDelta $ SCU.take matchLength s0.src) s0.pos
          in
            Right (match @@ (s0.pos .. pos')) /\ { src: SCU.drop matchLength s0.src, pos: pos' }
    _ ->
      Left LexUnexpected /\ s0
  where
  matchRegex = unsafeRegex ("^(?:" <> regexStr <> ")") unicode

parseKeyword :: String -> Maybe Keyword
parseKeyword = case _ of
  "fun" -> Just KW_fun
  "if" -> Just KW_if
  "then" -> Just KW_then
  "else" -> Just KW_else
  _ -> Nothing