module TAPL.TRecon.Syntax.Position
  ( emptyPos
  , emptyRange
  , module ReExport
  )
  where

import TAPL.STLC.Syntax.Position 
  ( SourcePhrase
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

emptyPos :: ReExport.SourcePos
emptyPos = { ln: 1, col: 1 }

emptyRange :: ReExport.SourceRange
emptyRange = { start: emptyPos, end: emptyPos }