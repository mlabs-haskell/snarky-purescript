module SnarkyPS.Lib.Circuit where

import Prelude hiding (Void)
import SnarkyPS.Lib.CircuitValue as C
import Data.Function.Uncurried
import Type.Proxy
import Partial
import Partial.Unsafe
import Data.Maybe
import Control.Promise

import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Bool

import Effect
import Effect.Aff

foreign import data Circuit :: Type -> Type -> Type

-- TODO: remove later
foreign import debug :: forall t. t -> Effect Unit

-- a is public, b is private
foreign import mkCircuit_ :: forall a b c. Fn3 (Fn2 a b c) (Provable a) (Provable b) (Circuit a b)

foreign import data Proof :: Type -> Type -> Type

-- TODO: Needs to be in aff or return a promise or both
foreign import prove_ :: forall a b. Fn3 (Circuit a b) a b (Promise (Proof a b))

foreign import bindToConstant :: Field -> Field

-- idk just a guess for debugging
foreign import proveArray :: forall a. Provable a -> Provable a

-- these should all  be Fn1s
newtype Provable :: Type -> Type
newtype Provable t = Provable {
    toFields ::  t -> Fields
  , toAuxiliary :: t -> (Array t) -- unused
  , fromFields :: Fields -> t
  , sizeInFields :: t -> SizeInFields
  , check :: t -> (Context Void)
  , toInput :: t -> HashInput
}

mkCircuit :: forall a b c
             . C.CircuitValue a
             => C.CircuitValue b
             => (a -> b -> Context Void)
             -> Circuit a b
mkCircuit f = runFn3 mkCircuit_ (mkFn2 f) (mkProvable @a) (mkProvable @b)

mkProvable :: forall @t. C.CircuitValue t => Provable t
mkProvable = Provable {
    toFields: C.toFields (Proxy :: Proxy t)
  , toAuxiliary: const []
  , fromFields:  (unsafePartial  fromJust) <<<  C.fromFields (Proxy :: Proxy t)
  , sizeInFields: const (C.sizeInFields (Proxy :: Proxy t))
  , check: C.check
  , toInput: C.toInput
}

-- TODO: Needs to run in aff or return a promise or both
prove :: forall a b. Circuit a b -> a -> b -> Aff (Proof a b)
prove c a b = toAff $ runFn3 prove_ c a b
