module SnarkyPS.Lib.Circuit where

import Prelude hiding (Void)
import SnarkyPS.Lib.CircuitValue as C
import Data.Function.Uncurried
import Type.Proxy
import Partial
import Partial.Unsafe
import Unsafe.Coerce
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

-- N.B. The `Circuit` arguments are normal PS types
foreign import mkCircuit_ :: forall pub priv pub' priv'
                          . Fn3 (Fn2 pub' priv' ZUnit) (Provable pub') (Provable priv') (Circuit pub priv)

foreign import data Proof :: Type -> Type -> Type

foreign import prove_ :: forall pub priv pub' priv'
                         . Fn3 (Circuit pub priv) pub' priv' (Promise (Proof pub priv))

foreign import data Provable :: Type -> Type

foreign import mkProvable :: forall (t :: Type). SizeInFields -> Provable t

foreign import provableSizeInFields :: forall t. Provable t -> Int

provable :: forall @t t'. CircuitValue t => AsFieldsOf t t' => Provable t'
provable = mkProvable $ sizeInFields (Proxy :: Proxy t)

zkProvable :: forall @t. CircuitValue t => Provable (AsFields t)
zkProvable = mkProvable $ sizeInFields (Proxy :: Proxy t)

mkCircuit :: forall pub priv pub' priv'
             . AsFieldsOf pub pub'
             => AsFieldsOf priv priv'
             => CircuitValue pub
             => CircuitValue priv
             => (pub' -> priv' -> ZkM Unit)
             -> Circuit pub priv
mkCircuit f = runFn3 mkCircuit_ (mkFn2 f') pubProvable privProvable
  where
    f' :: pub' -> priv' -> ZUnit
    f' pub' priv' = unFields $ unZkM (f pub' priv') $ \_ -> asFields zUnit
    privProvable =  provable @priv
    pubProvable  =  provable @pub

prove :: forall  (pub :: Type) (priv :: Type) (pub' :: Type) (priv' :: Type)
      . AsFieldsOf pub pub'
      => AsFieldsOf priv priv'
      => CircuitValue pub
      => CircuitValue priv
      => Circuit pub priv -> pub -> priv  -> Aff (Proof pub priv)
prove c pub priv = toAff $ runFn3 prove_ c pub' priv'
  where
    pub' :: pub'
    pub' = toInput pub

    priv' :: priv'
    priv' = toInput priv

toInput :: forall (t :: Type) (t' :: Type). CircuitValue t => AsFieldsOf t t' => t -> t'
toInput = unsafeCoerce <<< asFields
