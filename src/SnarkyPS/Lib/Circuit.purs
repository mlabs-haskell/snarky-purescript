module SnarkyPS.Lib.Circuit (Circuit, Proof, Provable, debug, mkCircuit, prove, zkProvable) where

import Prelude (Unit,($),(<<<))
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Type.Proxy (Proxy(Proxy))

import Effect (Effect)
import Effect.Aff (Aff)

import Control.Promise (Promise, toAff)

import Unsafe.Coerce (unsafeCoerce)

import SnarkyPS.Lib.CircuitValue.Class (class AsFieldsOf, class CircuitValue, AsFields, asFields, sizeInFields, unFields)
import SnarkyPS.Lib.CircuitValue.Data (ZkM, unZkM)
import SnarkyPS.Lib.Field (SizeInFields)
import SnarkyPS.Lib.FieldClasses (ZUnit, zUnit)

{- | Corresponds to a SnarkyJS `Circuit.Circuit` -}
foreign import data Circuit :: Type -> Type -> Type

-- TODO: remove later
foreign import debug :: forall t. t -> Effect Unit

-- N.B. The `Circuit` arguments are normal PS types
foreign import mkCircuit_ :: forall pub priv pub' priv'
                          . Fn3 (Fn2 pub' priv' ZUnit) (Provable pub') (Provable priv') (Circuit pub priv)

{- | Corresponds to a SnarkyJS `Proof` -}
foreign import data Proof :: Type -> Type -> Type

foreign import prove_ :: forall pub priv pub' priv'
                         . Fn3 (Circuit pub priv) pub' priv' (Promise (Proof pub priv))

foreign import data Provable :: Type -> Type

foreign import mkProvable :: forall (t :: Type). SizeInFields -> Provable t

{-
| Generates a `Provable zTy` where `zTy` is such that `AsFieldsOf ty zTy`
| for some `ty` supplied via a type application. This is very internal
| and has no applications for users.
-}
provable :: forall @ty zTy. CircuitValue ty => AsFieldsOf ty zTy => Provable zTy
provable = mkProvable $ sizeInFields (Proxy :: Proxy ty)

{-
| Generates a `Provable t` where `t` is an instance of `CircuitValue`
| (`t` is provided via type application).
|
| This is very internal and has no application for users.
-}
zkProvable :: forall @t. CircuitValue t => Provable (AsFields t)
zkProvable = mkProvable $ sizeInFields (Proxy :: Proxy t)

{-
| Construct a `Circuit pub priv` from a function
| `zPub -> zPriv -> ZkM Unit`, where
|    `AsFieldsOf pub zPub`
| and
|    `AsFieldsOf priv zPriv`
-}
mkCircuit :: forall pub priv zPub zPriv
             . AsFieldsOf pub zPub
             => AsFieldsOf priv zPriv
             => CircuitValue pub
             => CircuitValue priv
             => (zPub -> zPriv -> ZkM Unit)
             -> Circuit pub priv
mkCircuit f = runFn3 mkCircuit_ (mkFn2 f') pubProvable privProvable
  where
    f' :: zPub -> zPriv -> ZUnit
    f' zPub zPriv = unFields $ unZkM (f zPub zPriv) $ \_ -> asFields zUnit
    privProvable =  provable @priv
    pubProvable  =  provable @pub

{-
| Generate a proof from a circuit and its inputs.
-}
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
