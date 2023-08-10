module SnarkyPS.Lib.Hash (Hash, hashFields) where

import SnarkyPS.Lib.Field

foreign import data Hash :: Type

foreign import hashFields :: Array Field -> Hash
