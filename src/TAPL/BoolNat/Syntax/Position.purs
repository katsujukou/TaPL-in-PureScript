module TAPL.BoolNat.Syntax.Position where

import Prelude

import Data.Array (uncons)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU

type SourcePos = 
  { ln :: Int 
  , col :: Int 
  }

type SourceRange =
  { start :: SourcePos 
  , end :: SourcePos 
  }

span :: SourcePos -> SourcePos -> SourceRange
span = { start:_, end: _ }

infix 5 span as ..

range :: SourceRange -> SourceRange -> SourceRange 
range { start } { end } = start..end

infix 5 range as ~

type SourcePhrase a =
  { it :: a 
  , at :: SourceRange
  }

at :: forall a. a -> SourceRange -> SourcePhrase a 
at it a = { it, at: a }

infix 6 at as @@ 

mapPhrase :: forall a b. (a -> b) -> SourcePhrase a -> SourcePhrase b 
mapPhrase f ph = ph { it = f ph.it }

data ColumnDelta = DeltaCarriage | DeltaChars Int

derive instance Eq ColumnDelta
derive instance Generic ColumnDelta _ 
instance Show ColumnDelta where
  show = genericShow

-- With this implementation, the associativity does not hold,
-- so we shouldn't make ColumnDelta as an instance of Semigroup.
-- Counter example: 
--   ((DeltaChars n1 ++ DeltaCarriage) ++ DeltaChars n2)
--     == DeltaCarriage ++ DeltaChars n2 
--     == DeltaChars n2
--     /= DeltaChars n1 ++ (DeltaCarriage ++ DeltaChars n2)
--     == DeltaChars n1 ++ DeltaChars n2
--     == DeltaChars (n1 + n2)
appendColumnDelta :: ColumnDelta -> ColumnDelta -> ColumnDelta
appendColumnDelta = case _, _ of
    _, DeltaCarriage -> DeltaCarriage
    DeltaCarriage, d2 -> d2
    DeltaChars n1, DeltaChars n2 -> DeltaChars (n1 + n2)

infixl 4 appendColumnDelta as ++ 

newtype SourceDelta = SourceDelta { dl :: Int, dc :: ColumnDelta }

instance Show SourceDelta where 
  show (SourceDelta dlt) = "(SourceDelta " <> show dlt <> ")"
appendSourceDelta :: SourceDelta -> SourceDelta -> SourceDelta
appendSourceDelta (SourceDelta d1) (SourceDelta d2) = SourceDelta case d1, d2 of
    { dl: 0 }, { dl: 0 } -> { dl: 0, dc: d1.dc ++ d2.dc }
    { dl: 0 }, _ -> d2 
    _, { dl: 0 } -> { dl: d1.dl, dc: d1.dc ++ d2.dc }
    _, _ -> { dl: d1.dl + d2.dl, dc: d2.dc }

infixl 4 appendSourceDelta as +*

advancePos :: SourceDelta -> SourcePos -> SourcePos
advancePos delta pos = case delta of
  SourceDelta { dl: 0, dc } -> pos { col = applyColumnDelta dc pos.col } 
  SourceDelta { dl, dc } -> advancePos (SourceDelta { dl: dl - 1, dc }) (pos { ln = pos.ln + 1, col = 1 })
  where
    applyColumnDelta DeltaCarriage _ = 1 
    applyColumnDelta (DeltaChars n) c = c + n

charDelta :: Char -> SourceDelta 
charDelta = SourceDelta <<< case _ of
  '\n' -> { dl: 1, dc: DeltaChars 0 }
  '\r' -> { dl: 0, dc: DeltaCarriage }
  _ -> { dl: 0, dc: DeltaChars 1 }

stringDelta :: String -> SourceDelta 
stringDelta = SCU.toCharArray >>> go (SourceDelta { dl: 0, dc: DeltaChars 0 })
  where
    go delta = uncons >>> case _ of
      Nothing -> delta 
      Just {head, tail} -> go (delta +* charDelta head) tail

printPos :: SourcePos -> String 
printPos {ln,col} = show ln <> "," <>  show col

printRange :: SourceRange -> String 
printRange { start, end } = show start <> " - " <> show end