module TAPL.STLCEx.Syntax.Utils where

import Prelude

import Data.String.CodeUnits as SCU
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)

isUppercase :: Char -> Boolean
isUppercase = SCU.singleton >>> Re.test (unsafeRegex """[A-Z]""" unicode) 

isLowercase :: Char -> Boolean
isLowercase = SCU.singleton >>> Re.test (unsafeRegex """[a-z]""" unicode) 