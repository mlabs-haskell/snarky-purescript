module SnarkyPS.Lib.Hash (Hash, hashFields, hashFields') where

import Prelude ((<<<))
import SnarkyPS.Lib.Field
import Unsafe.Coerce

foreign import data Hash :: Type

foreign import hashFields :: Array Field -> Hash

hashFields' :: Fields -> Hash
hashFields' = hashFields <<< unsafeCoerce
