module SnarkyPS.Lib.Provable where

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.CircuitValue as C
import Effect
import Prelude
import Data.Function.Uncurried
import Type.Proxy
import Partial
import Partial.Unsafe
import Data.Maybe


foreign import data CircuitMain :: Type

foreign import debug :: forall t. t -> Effect Unit

-- these should all  be Fn1s
newtype Provable :: Type -> Type
newtype Provable t = Provable {
    toFields :: Fn1 t (Array Field)
  , toAuxiliary :: Fn1 t (Array t) -- unused
  , fromFields :: Fn1 (Array Field) t
  , sizeInFields :: Fn1 t Int
  , check :: Fn1 t (Effect Unit) -- TODO: unsafePerformEffect this?
}


mkProvable :: forall @t. C.CircuitValue t => Provable t
mkProvable = Provable {
    toFields: mkFn1 $ C.toFields (Proxy :: Proxy t)
  , toAuxiliary: mkFn1 $ const []
  , fromFields: mkFn1 $ (unsafePartial  fromJust) <<<  C.fromFields (Proxy :: Proxy t)
  , sizeInFields: mkFn1 $ const (C.sizeInFields (Proxy :: Proxy t))
  , check: mkFn1 $ C.check
}

test :: Fn2 Bool Bool Bool
test = mkFn2 $ \b1 b2 -> b1 #== b2
