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

import Effect
import Effect.Aff

foreign import data Circuit :: Type -> Type -> Type

-- TODO: remove later
foreign import debug :: forall t. t -> Effect Unit

-- a is public, b is private
foreign import mkCircuit_ :: forall a b
                          . Fn3 (Fn2 a b Assertion) (Provable a) (Provable b) (Circuit a b)

foreign import data Proof :: Type -> Type -> Type

-- TODO: Needs to be in aff or return a promise or both
foreign import prove_ :: forall a b a' b'
                         . Fn3 (Circuit a b) a' b' (Promise (Proof a b))

foreign import bindToConstant :: Field -> Field

foreign import data Provable :: Type -> Type

foreign import mkProvable :: forall (t :: Type). SizeInFields -> Provable t

provableSized :: forall @t. Sized t => Provable t
provableSized = mkProvable $ sizeInFields_ (Proxy :: Proxy t)

provable :: forall @t. CircuitValue t => Provable t
provable = mkProvable $ sizeInFields (Proxy :: Proxy t)

mkCircuit :: forall a b
             . Sized a
             => Sized b
             => (a -> b -> Assertion)
             -> Circuit a b
mkCircuit f = runFn3 mkCircuit_ (mkFn2 f) privProvable pubProvable
  where
    privProvable = mkProvable $ sizeInFields_ (Proxy :: Proxy a)
    pubProvable  = mkProvable $ sizeInFields_ (Proxy :: Proxy a)

prove :: forall (f :: Row Type -> Type)
          (f' :: Row Type -> Type)
          (g :: Row Type -> Type)
          (g' :: Row Type -> Type)
          (priv :: Row Type)
          (pub :: Row Type)
          (zPriv :: Row Type)
          (zPub :: Row Type)
      . ZkData f f' priv zPriv
      => ZkData g g' pub zPub
      => Circuit (f' zPriv) (g' zPub) -> f priv ->  g pub -> Aff (Proof (f' zPriv) (g' zPub))
prove c priv pub = toAff $ runFn3 prove_ c zkPriv zkPub
  where
    zkPriv :: f' zPriv
    zkPriv = zkMorph priv

    zkPub :: g' zPub
    zkPub = zkMorph pub
