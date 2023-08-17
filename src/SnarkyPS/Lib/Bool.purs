module SnarkyPS.Lib.Bool (
    assertTrue
  , equalsBool
  , assertEqBool
  , toFieldBool
  , fromFieldBool
  , checkBool
  ) where

import SnarkyPS.Lib.Field (Field, Bool, SizeInFields)
import SnarkyPS.Lib.Context (Assertion)
{-
  Bool functions
-}

{- Probably not needed by users

foreign import isConstantBool :: Bool -> Boolean

foreign import toStringBool :: Bool -> String
-}

foreign import toFieldBool :: Bool -> Field

foreign import assertEqBool :: String -> Bool -> Bool -> Assertion

foreign import assertTrue :: String -> Bool -> Assertion

foreign import assertFalse :: String -> Bool -> Assertion

foreign import equalsBool :: Bool -> Bool -> Bool

foreign import sizeInFieldsBool :: Bool -> SizeInFields

foreign import toFieldsBool :: Bool -> Array Field

foreign import checkBool :: Bool -> Assertion

foreign import fromFieldBool :: Field -> Bool
