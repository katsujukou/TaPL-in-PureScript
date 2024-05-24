module TAPL.REPL.Node.ReadLine where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits as SCU
import Data.String.Regex as Re
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (endsWith, trimEnd)
import Data.Zipper (Zipper)
import Data.Zipper as Zipper
import Data.Zipper.NonEmpty (NonEmptyZipper)
import Data.Zipper.NonEmpty as NonEmptyZipper
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn2)
import Foreign (Foreign)
import TAPL.REPL.Node.Console as Console

times :: forall a. Int -> (a -> a) -> a -> a
times n f = go n 
  where
  go 0 x = x 
  go i x = go (i - 1) (f x)

type KeyBuffer = NonEmptyZipper (Zipper Char)

emptyBuffer :: KeyBuffer
emptyBuffer = NonEmptyZipper.singleton (Zipper.empty)

ln :: KeyBuffer -> Int 
ln = NonEmptyZipper.left >>> L.length >>> (_ + 1)

col :: KeyBuffer -> Int 
col = NonEmptyZipper.extract >>> Zipper.left >>> L.length

lineLength :: KeyBuffer -> Int 
lineLength = NonEmptyZipper.extract >>> Zipper.length 

lines :: KeyBuffer -> Array String
lines = NonEmptyZipper.toArray >>> map (Zipper.toArray >>> SCU.fromCharArray)

toStringLine :: KeyBuffer -> String 
toStringLine = NonEmptyZipper.extract >>> Zipper.toArray >>> SCU.fromCharArray 

data KeyName 
  = KeyString String 
  | KeyCode Int 
  | KeyUndefined 

derive instance Eq KeyName 
derive instance Generic KeyName _ 
instance Show KeyName where
  show = genericShow

type Key =
  { sequence :: String
  , name :: Foreign
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

foreign import getRawInputImpl :: EffectFn2
  (EffectFn2 String Key Unit)
  (EffectFn1 Key Boolean)
  (Promise String)

getRawInput :: Maybe (String -> String) -> Aff String
getRawInput hook = do
  state <- liftEffect $ (Ref.new emptyBuffer)
  _ <- toAffE $ 
    runEffectFn2 
      getRawInputImpl 
      (handleStroke state) 
      (shouldClose state)

  buf <- liftEffect $ 
    Ref.read state 
      <#> NonEmptyZipper.toArray
      >>> Array.foldMap (Zipper.toArray >>> SCU.fromCharArray)
  pure $ replaceAll (Pattern "\\") (Replacement "\n") buf

  where
  handleStroke :: Ref KeyBuffer ->  EffectFn2 String Key Unit 
  handleStroke state = mkEffectFn2 \_ k -> do
    case mkKeyName k.name of
      KeyString kn
        | kn == "up" -> moveCursor "up" state
        | kn == "down" -> moveCursor "down" state
        | kn == "left" -> moveCursor "left" state
        | kn == "right" -> moveCursor "right" state
        | kn == "backspace" -> do
            s0 <- Ref.read state
            Ref.write (NonEmptyZipper.update Zipper.delete s0) state
            printLn state
        | kn == "return" -> do
            s0 <- Ref.read state
            when (NonEmptyZipper.isLast s0) do
              Console.write "\x1b[1S\x1b[1E"
              Ref.write (NonEmptyZipper.moveLeft $ NonEmptyZipper.cons Zipper.empty s0) state
        | isPrintableChar k -> do
            putchars state (SCU.toCharArray k.sequence)
            printLn state
      KeyUndefined
        | not k.ctrl
        , not k.meta 
        , not k.shift -> do
            putchars state (SCU.toCharArray k.sequence)
            printLn state
      _ -> pure unit

  shouldClose :: Ref KeyBuffer -> EffectFn1 Key Boolean
  shouldClose state = mkEffectFn1 \k -> do
    let keyname = mkKeyName k.name
    if keyname /= KeyString "return" then pure false 
    else do 
      buf <- Ref.read state
      let lastline = SCU.fromCharArray $ Zipper.toArray $ NonEmptyZipper.extract buf
      if ("\\" `endsWith` trimEnd lastline) then do 
        Ref.write (NonEmptyZipper.moveRight buf) state
        pure false 
      else do
        Console.write ("\x1b[" <> show (5 + L.length (NonEmptyZipper.right buf)) <> "E")
        pure true

  printLn state = do
    s <- NonEmptyZipper.extract <$> Ref.read state
    let 
      line = SCU.fromCharArray $ Zipper.toArray s
    Console.write "\x1b[1G\x1b[0K"
    Console.write (hook # maybe line ( _ $ line))
    let 
      rLen = L.length $ Zipper.right s
    when (rLen /= 0) do
      Console.write $ "\x1b[" <> show rLen <> "D"
 
  moveCursor direction s = do
    buf <- Ref.read s
    case direction of 
      "left" -> do
        Ref.write (NonEmptyZipper.update Zipper.moveLeft buf) s
        unless (col buf <= 0) do
          Console.write "\x1b[1D"
      "right" -> do
        Ref.write (NonEmptyZipper.update Zipper.moveRight buf) s
        unless (Zipper.isLast $ NonEmptyZipper.extract buf) do
          Console.write "\x1b[1C"
      "up" -> do
        unless (ln buf <= 1) do
          Console.write "\x1b[1A"
        -- Adjust cursol column
        let
          col0 = col buf
          buf' = NonEmptyZipper.moveLeft buf
          col1 = col buf'
          buf'' =
            if col0 > col1 then
              NonEmptyZipper.update ((col0 - col1) `times` Zipper.moveRight) buf'
            else 
              NonEmptyZipper.update ((col1 - col0) `times` Zipper.moveLeft) buf'
        Ref.write buf'' s 
        when (col0 >= lineLength buf'') do
          Console.write $ "\x1b[" <> (show (lineLength buf'' + 1)) <> "G"

      "down" -> do 
        Console.write "\x1b[1B"
        -- Adjust cursol column
        let
          col0 = col buf
          buf' = NonEmptyZipper.moveRight buf
          col1 = col buf'
          buf'' = 
            if col0 > col1 then 
              NonEmptyZipper.update ((col0 - col1) `times` Zipper.moveRight) buf'
            else 
              NonEmptyZipper.update ((col1 - col0) `times` Zipper.moveLeft) buf'
        Ref.write buf'' s
        when (col0 >= lineLength buf'') do
          Console.write $ "\x1b[" <> (show (lineLength buf'' + 1)) <> "G"
          
      _ -> pure unit

  isPrintableChar :: Key -> Boolean
  isPrintableChar k = not k.ctrl
    && not k.meta 
    && Re.test (unsafeRegex """(\w|\s|\\)""" global) k.sequence

  putchars :: Ref KeyBuffer -> Array Char -> Effect Unit
  putchars s chars = do
    s0 <- Ref.read s
    let s1 = Array.foldl (\buf ch -> NonEmptyZipper.update (Zipper.cons ch) buf) s0 chars
    Ref.write s1 s 

compareLeft :: String -> String -> Int
compareLeft xs ys = go 0 (L.fromFoldable $ SCU.toCharArray xs) (L.fromFoldable $ SCU.toCharArray ys)
  where
    go i = case _, _ of
      Cons hd1 tl1, Cons hd2 tl2
        | hd1 == hd2 -> go (i + 1) tl1 tl2 
      _, _ -> i  
  
mkKeyName :: Foreign -> KeyName
mkKeyName = runFn4 mkKeyNameImpl KeyCode KeyString KeyUndefined 

foreign import mkKeyNameImpl :: Fn4
  (Int -> KeyName)
  (String -> KeyName)
  KeyName
  Foreign
  KeyName