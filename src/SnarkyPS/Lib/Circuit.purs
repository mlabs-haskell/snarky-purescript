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
import SnarkyPS.Lib.CircuitValue.Data
import SnarkyPS.Lib.CircuitValue.Class

import Effect
import Effect.Aff

foreign import data Circuit :: Type -> Type -> Type

-- TODO: remove later
foreign import debug :: forall t. t -> Effect Unit

-- a is public, b is private
foreign import mkCircuit_ :: forall pub priv
                          . Fn3 (Fn2 (Zk pub) (Zk priv) Assertion) (Provable (Zk pub)) (Provable (Zk priv)) (Circuit pub priv)

foreign import data Proof :: Type -> Type -> Type

-- TODO: Needs to be in aff or return a promise or both
foreign import prove_ :: forall pub priv
                         . Fn3 (Circuit pub priv) (Zk pub) (Zk priv) (Promise (Proof pub priv))

foreign import data Provable :: Type -> Type

foreign import mkProvable :: forall (t :: Type). SizeInFields -> Provable t

foreign import provableSizeInFields :: forall t. Provable t -> Int

zkProvable :: forall @t. CircuitValue t => Provable (Zk t)
zkProvable = mkProvable $ sizeInFields (Proxy :: Proxy t)


mkCircuit :: forall pub priv
             . CircuitValue pub
             => CircuitValue priv
             => (Zk pub -> Zk priv -> Assertion)
             -> Circuit pub priv
mkCircuit f = runFn3 mkCircuit_ (mkFn2 f) pubProvable privProvable
  where
    privProvable =  (zkProvable @priv)
    pubProvable  =  (zkProvable @pub)

prove :: forall  (pub :: Type) (priv :: Type)
      . CircuitValue pub
      => CircuitValue priv
      => Circuit pub priv -> pub -> priv  -> Aff (Proof pub priv)
prove c pub priv = toAff $ runFn3 prove_ c (toZk pub) (toZk priv)
