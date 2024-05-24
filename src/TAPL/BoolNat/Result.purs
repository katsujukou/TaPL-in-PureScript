module TAPL.BoolNat.Result where

import Data.Either (Either(..))
import TAPL.BoolNat.Error (Error)

type Result a = Either Error a 

throwError  :: forall a. Error -> Result a 
throwError = Left