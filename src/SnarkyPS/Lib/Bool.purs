module SnarkyPS.Lib.Bool where

import Prelude hiding (Void)
import Data.Boolean
import Data.Function.Uncurried

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Context
{-
  Bool functions
-}

{- Probably not needed by users

foreign import isConstantBool :: Bool -> Boolean

foreign import toStringBool :: Bool -> String
-}

foreign import toFieldBool :: Bool -> Field

foreign import notBool :: Bool -> Bool

foreign import andBool :: Bool -> Bool -> Bool

foreign import orBool :: Bool -> Bool -> Bool

foreign import assertEqBool :: String -> Bool -> Bool -> Assertion

foreign import assertTrue :: String -> Bool -> Assertion

foreign import assertFalse :: String -> Bool -> Assertion

foreign import equalsBool :: Bool -> Bool -> Bool

foreign import sizeInFieldsBool :: Bool -> SizeInFields

foreign import toFieldsBool :: Bool -> Array Field

foreign import checkBool :: Bool -> Assertion

foreign import fromFieldBool :: Field -> Bool
