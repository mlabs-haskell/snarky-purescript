module SnarkyPS.Lib.Bool where

import Data.Boolean

foreign import data Zk :: Type -> Type

foreign import data Bool :: Type

foreign import newBool :: Boolean -> Bool

foreign import boolToString :: Bool -> String
