module TAPL.STLCEx.Syntax.Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.Partial as ArrayP
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import TAPL.STLCEx.Syntax.Error (ParseError(..))
import TAPL.STLCEx.Syntax.Lexer (LexerState, runLexer, tokenize)
import TAPL.STLCEx.Syntax.Position (SourcePhrase, mapPhrase, (..), (@@), (~))
import TAPL.STLCEx.Syntax.Types (Accessor(..), Ann, Binder(..), Const(..), Expr(..), Keyword(..), Label(..), Labeled(..), Pattern(..), SourceToken, Token(..), Type_(..), accessorAnn, exprAnn, patternAnn, typeAnn)
import TAPL.STLCEx.Types (Ident)

type ParserState = 
  { lexerState :: LexerState
  }

type ParseResult a = Either ParseError a 


recoverable :: ParseError -> Boolean 
recoverable = case _ of
  LexUnexpected -> true 
  UnexpectedToken TokEOF -> true
  _ -> false

recover :: ParserState -> ParseError -> Either ParseError ParserState 
recover s0 = case _ of
  UnexpectedToken _ -> Right s0
  e -> Left e  

newtype Parser a = Parser (ParserState -> ParseResult a /\ ParserState)

instance Functor Parser where
  map f (Parser k) = Parser \s0 -> lmap (map f) (k s0)

instance Apply Parser where 
  apply (Parser k1) (Parser k2) = Parser \s0 -> 
    case k1 s0 of 
      Left e /\ s1 -> Left e /\ s1 
      Right f /\ s1 -> lmap (map f) (k2 s1)

instance Applicative Parser where
  pure a = Parser \s0 -> Right a /\ s0 

instance Bind Parser where
  bind (Parser k1) f = Parser \s0 -> 
    case k1 s0 of 
      Left e /\ s1 -> Left e /\ s1
      Right a /\ s1 -> let Parser k2 = f a in k2 s1

instance Monad Parser 

instance Alt Parser where 
  alt (Parser k1) (Parser k2) = Parser \s0 ->
    case k1 s0 of
      Right a /\ s1 -> Right a /\ s1
      Left err /\ s1 
        | s1 == s0 -> k2 s1 
        | otherwise -> Left err /\ s1 

instance Lazy (Parser a) where
  defer l = Parser \s -> let Parser k = l unit in k s 

execParser :: forall a. Parser a -> ParserState -> ParseResult a 
execParser p s0 = fst $ runParser p s0  

runParser :: forall a. Parser a -> ParserState -> ParseResult a /\ ParserState
runParser (Parser k) s0 = k s0

initialState :: String -> ParserState 
initialState src = 
  { lexerState: { src, pos: { ln: 1, col: 1 } }
  }

failwith :: forall a. ParseError -> Parser a 
failwith err = Parser \s -> Left err /\ s 

optional :: forall a. Parser a -> Parser (Maybe a)
optional (Parser p) = Parser \s -> case p s of
  Right a /\ s1 -> Right (Just a) /\ s1 
  Left e /\ s1 
    | Right s' <- recover s1 e -> Right Nothing /\ s' 
    | otherwise -> Left e /\ s1 
  
many :: forall a. Parser a ->  Parser (Array a)
many (Parser p) = Parser \s -> go [] s
  where
    go acc s0 = case p s0 of
      Right a /\ s1 -> go (acc <> [a]) s1 
      Left e /\ s1 
        | Right s1' <- recover s1 e -> Right acc /\ s1'
        | otherwise -> Left e /\ s1

try :: forall a. Parser a -> Parser a
try (Parser p) = Parser \s -> case p s of 
  Right a /\ s1 -> Right a /\ s1 
  Left e /\ _ -> Left e /\ s
  
followedBy :: forall a b. Parser a -> Parser b -> Parser (a /\ b)
followedBy p1 p2 = try ((/\) <$> p1 <*> p2) 

delimBy :: forall a. Parser SourceToken -> Parser a -> Parser (Array a)
delimBy dlm p = do
  a <- optional p
  case a of
    Nothing -> pure []
    Just a0 -> do 
      as <- many (dlm `followedBy` p)
      pure $ Array.cons a0 (map snd as) 

tokenSuchThat :: (Token -> Boolean) -> Parser SourceToken 
tokenSuchThat f = Parser \s -> case runLexer tokenize s.lexerState of
  Left err /\ s' -> Left err /\ (s {lexerState = s'})
  Right st /\ s' 
    | f st.it -> Right st /\ (s {lexerState = s'}) 
    | otherwise -> Left (UnexpectedToken st.it) /\ s

token :: Token -> Parser SourceToken 
token t = tokenSuchThat (_ == t)

-- tokens 
leftParens :: Parser SourceToken
leftParens = token TokLeftParens

rightParens :: Parser SourceToken 
rightParens = token TokRightParens

leftBrace :: Parser SourceToken
leftBrace = token TokLeftBrace

rightBrace :: Parser SourceToken 
rightBrace = token TokRightBrace

equal :: Parser SourceToken 
equal = token TokEqual

dot :: Parser SourceToken
dot = token TokDot

comma :: Parser SourceToken 
comma = token TokComma

colon :: Parser SourceToken
colon = token TokColon 

index :: Parser SourceToken 
index = tokenSuchThat case _ of 
  TokIndex _ -> true 
  TokProperty _ -> true 
  _ -> false

underscore :: Parser SourceToken 
underscore = token TokUnderscore

star :: Parser SourceToken 
star = token TokStar

rightArrow :: Parser SourceToken 
rightArrow = token TokRightArrow

semicolon :: Parser SourceToken 
semicolon = token TokSemicolon

fun :: Parser SourceToken 
fun = token (TokReserved KW_fun)

let_ :: Parser SourceToken
let_ = token (TokReserved KW_let)

rec :: Parser SourceToken
rec = token (TokReserved KW_rec)

and :: Parser SourceToken
and = token (TokReserved KW_and)

in_ :: Parser SourceToken
in_ = token (TokReserved KW_in)

if_ :: Parser SourceToken 
if_ = token (TokReserved KW_if)

then_ :: Parser SourceToken 
then_ = token (TokReserved KW_then)

else_ :: Parser SourceToken 
else_ = token (TokReserved KW_else)

true_ :: Parser SourceToken
true_ = token (TokBool true)

false_ :: Parser SourceToken
false_ = token (TokBool false)

anyToken :: Parser SourceToken 
anyToken = tokenSuchThat (const true)

nat :: Parser (SourcePhrase Int)
nat = do 
  tok <- nat_
  pure case tok of
    { at, it: TokNat n } -> n @@ at
    _ -> unsafeCrashWith "Impossible"
  where  
  nat_ :: Parser SourceToken
  nat_ = tokenSuchThat case _ of 
    TokNat _ -> true
    _ -> false 

unit_ :: Parser (SourceToken)
unit_ = token TokUnit

ident :: Parser (SourcePhrase Ident) 
ident = ident_ <#> mapPhrase case _ of 
  TokIdent id -> id 
  _ -> unsafeCrashWith "Impossible"

ident_ :: Parser SourceToken 
ident_ = tokenSuchThat case _ of 
  TokIdent _ -> true 
  _ -> false

parensed :: forall a. Parser (SourcePhrase a) -> Parser (SourcePhrase a)
parensed p = do
  tok1 <- leftParens
  a <- p
  tok2 <- rightParens 
  pure $ 
    { at: tok1.at ~ tok2.at, it: a.it }

parseExpr :: Parser (Expr Ann)
parseExpr = defer \_ -> do 
  parseExprApp

parseExprApp :: Parser (Expr Ann) 
parseExprApp = defer \_ -> do
  exp1 <- parseExpr1
  expArgs <- many parseExpr1 
  case NonEmptyArray.fromArray expArgs of 
    Nothing -> pure exp1
    Just expArgs' -> do
      let 
        lst = NonEmptyArray.last expArgs' 
      pure $ ExprApp 
        {pos: (exprAnn exp1).pos.start .. (exprAnn lst).pos.end } 
        exp1 
        expArgs'

parseExpr1 :: Parser (Expr Ann)
parseExpr1 = defer \_ ->  
  parseExprAbs 
    <|> parseExpr2 

parseExprAbs :: Parser (Expr Ann)
parseExprAbs = defer \_ -> do
  { at: pos1 } <- fun
  arg <- parseBinder
  args <- many parseBinder
  _ <- rightArrow
  body <- parseExpr
  pure $ ExprAbs 
    {pos: pos1 ~ (exprAnn body).pos }
    (NonEmptyArray.cons' arg args)
    body

parseExpr2 :: Parser (Expr Ann)
parseExpr2 = defer \_ -> do 
  parseExprIf
    <|> parseExprLet
    <|> parseExpr3

parseExprLet :: Parser (Expr Ann)
parseExprLet = defer \_ -> do
  {at:pos1} <- let_
  binder <- parseBinder
  _ <- equal
  exp <- parseExpr
  _ <- in_
  body <- parseExpr
  pure $ ExprLet 
    {pos:pos1 ~ (exprAnn body).pos}
    binder 
    exp
    body
  
parseExprIf :: Parser (Expr Ann)
parseExprIf = defer \_ -> do
  {at:pos1} <- if_
  t1 <- parseExpr
  _ <- then_
  t2 <- parseExpr 
  _ <-  else_ 
  t3 <- parseExpr
  let pos2 = (exprAnn t3).pos
  pure $ ExprIf {pos: pos1.start .. pos2.end} t1 t2 t3  

parseExpr3 :: Parser (Expr Ann)
parseExpr3 = defer \_ -> do
  expr <- parseExprAtom
  (parseExprAccess expr)
    <|> pure expr

parseExprAccess :: Expr Ann -> Parser (Expr Ann)
parseExprAccess exp = defer \_ -> do
  acs <- try do
    acsHd <- parseAccessor
    acsTl <- many parseAccessor
    pure $ NonEmptyArray.cons' acsHd acsTl
  pure $ ExprAccess 
    {pos: (exprAnn exp).pos ~ (accessorAnn $ NonEmptyArray.last acs).pos } 
    exp
    acs

parseAccessor :: Parser (Accessor Ann)
parseAccessor = defer \_ -> do
  tok1 <- index
  case tok1.it of 
    TokIndex n -> pure $ AcsIndex {pos:tok1.at} n
    TokProperty prop -> pure $ AcsProperty {pos:tok1.at} (Label prop)
    t -> failwith (UnexpectedToken t) 

parseExprAtom :: Parser (Expr Ann)
parseExprAtom = defer \_ -> do
  parseConst
  <|> parseExprIdent
  <|> parseExprRecord
  <|> parseExprTuple
  <|> parseParensExpr

parseExprTuple :: Parser (Expr Ann)
parseExprTuple = defer \_ -> try do
  {at:pos1} <- leftBrace
  exprs <- delimBy comma parseExpr 
  {at:pos2} <- rightBrace
  pure $ ExprTuple {pos:pos1 ~ pos2} exprs 

parseExprRecord :: Parser (Expr Ann)
parseExprRecord = defer \_ -> try do
  {at:pos1} <- leftBrace
  expFlds <- delimBy comma parseExprRecordField
  {at:pos2} <- rightBrace
  pure $ ExprRecord {pos:pos1 ~ pos2} expFlds 
  
parseExprRecordField :: Parser (Labeled (Expr Ann)) 
parseExprRecordField = defer \_ -> do 
  ((lbl /\ _) /\ exp) <- ident
    `followedBy` equal
    `followedBy` parseExpr
  pure $ Labeled (Label lbl.it)  exp
parseParensExpr :: Parser (Expr Ann)
parseParensExpr = defer \_ -> do 
  {at:pos1} <- leftParens
  exp <- parseExpr 
  {at:pos2} <- rightParens
  pure $ map (\_ -> { pos: pos1.start .. pos2.end }) exp

parseBinder :: Parser (Binder Ann)
parseBinder = defer \_ -> do
  parseBinder1 
  <|> parseBinderParens

parseBinder1 :: Parser (Binder Ann)
parseBinder1 = defer \_ -> do
  p <- parsePattern 
  mbType <- optional (colon *> parseType)
  pure $ Binder (ann p mbType) p mbType
  where
  ann p = case _ of 
    Nothing -> patternAnn p 
    Just typ -> { pos: (patternAnn p).pos ~ (typeAnn typ).pos }

parseBinderParens :: Parser (Binder Ann)
parseBinderParens = defer \_ -> leftParens *> parseBinder <* rightParens

parsePattern :: Parser (Pattern Ann)
parsePattern = defer \_ -> do
  parsePatWildcard 
  <|> parsePatVar

parsePatWildcard :: Parser (Pattern Ann)
parsePatWildcard = defer \_ -> do
  {at:pos} <- underscore
  pure $ PatWildcard {pos}

parsePatVar :: Parser (Pattern Ann)
parsePatVar = defer \_ -> do
  id <- ident
  pure $ PatVar {pos:id.at} id.it

parseType :: Parser (Type_ Ann)
parseType = defer \_ -> do
  typ <- parseType1
  typs <- many (rightArrow *> parseType1)
  case NonEmptyArray.fromArray typs of 
    Nothing -> pure typ
    Just ts -> do
      let { init, last } = NonEmptyArray.unsnoc ts 
      pure $
        TFun 
          { pos: (typeAnn typ).pos ~ (typeAnn last).pos } 
          (NonEmptyArray.cons' typ init)
          last

parseType1 :: Parser (Type_ Ann)
parseType1 = defer \_ -> do
  t1 <- parseTypeAtom
  ts <- many (star *> parseTypeAtom)
  case ts of
    [] -> pure t1 
    _ -> 
      let 
        last = unsafePartial (ArrayP.last ts) 
      in 
        pure $ TTup 
          { pos: (typeAnn t1).pos ~ (typeAnn last).pos } 
          (Array.cons t1 ts)

parseTypeAtom :: Parser (Type_ Ann)
parseTypeAtom = defer \_ -> do
  parseTypeFree
  <|> parseTypeRecord
  <|> parseParensType 

parseTypeFree :: Parser (Type_ Ann)
parseTypeFree = defer \_ -> do
  name <- ident
  pure $ TFree {pos: name.at} name.it


parseTypeRecord :: Parser (Type_ Ann)
parseTypeRecord = defer \_ -> do 
  {at:pos1} <- leftBrace
  typs <- delimBy comma parseRecordTypeField
  {at:pos2} <- rightBrace
  pure $ TRecord {pos:pos1 ~ pos2} (map _.it typs)

parseRecordTypeField :: Parser (SourcePhrase (Labeled (Type_ Ann)))
parseRecordTypeField = defer \_ -> do
  ((lbl /\ _) /\ typ) <- ident
    `followedBy` colon
    `followedBy` parseType 
  pure $ 
    { it: Labeled (Label lbl.it) typ
    , at: lbl.at ~ (typeAnn typ).pos 
    }

parseParensType :: Parser (Type_ Ann)
parseParensType = defer \_ -> do
  {at:pos1} <- leftParens
  typ <- parseType 
  {at:pos2} <- rightParens 
  pure $ TParens 
    { pos: pos1 ~ pos2}
    typ 

parseExprIdent :: Parser (Expr Ann)
parseExprIdent = defer \_ -> do
  id <- ident
  pure $ ExprIdent {pos: id.at} id.it

parseConst :: Parser (Expr Ann)
parseConst = defer \_ -> do 
  (true_ <#> \{at} -> ExprConst {pos:at} CstTrue)
  <|> (false_ <#> \{at} -> ExprConst {pos:at} CstFalse) 
  <|> (nat <#> natOfInt)
  <|> (unit_ <#> \{at} -> ExprConst {pos:at} CstUnit)

natOfInt :: SourcePhrase Int -> Expr Ann
natOfInt { at, it } = ExprConst { pos: at } (CstNat it)