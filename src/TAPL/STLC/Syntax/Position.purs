module TAPL.STLC.Syntax.Position
  ( module ReExport 
  ) where

import TAPL.BoolNat.Syntax.Position 
  (SourcePhrase
  , SourcePos
  , SourceRange
  , advancePos
  , span
  , (..)
  , at
  , (@@)
  , range 
  , (~)
  , charDelta
  , stringDelta 
  , mapPhrase
  , printPos 
  , printRange
  ) 
  as ReExport
