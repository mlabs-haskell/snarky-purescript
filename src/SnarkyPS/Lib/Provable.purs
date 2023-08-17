module SnarkyPS.Lib.Provable (witness, zkIf) where

import Prelude (Unit, unit, ($))

import SnarkyPS.Lib.Circuit (Provable, zkProvable)
import SnarkyPS.Lib.CircuitValue.Class (class CircuitValue, AsFields, asFields, unFields)
import SnarkyPS.Lib.Field (Bool)

foreign import zkIf_ :: forall t. Bool -> Provable t -> t -> t -> t

foreign import zkIfI_ :: forall t. Bool -> t -> t -> t

foreign import witness_ :: forall t. Provable t -> (Unit -> t) -> t

foreign import logAndThen :: forall t res. t -> res -> res

foreign import logAndThen_ :: forall t res. t -> res -> res

foreign import log :: forall t. t -> Unit

foreign import log_ :: forall t. t -> Unit

{-
| Construct a value in the Circuit. (This has ramifications for the proof, see
| the SnarkyJS documentation for details)
|
| A typical use for this is to perform a computation that
| is forbidden in a circuit context, but requires inputs
| that can only be accessed from within a circuit context
| (e.g. one that uses a *real* `if/then` instead of `zkIf`)
|
| Users of the DSL should not ever need this.
-}
witness :: forall (t :: Type). CircuitValue t => (Unit -> t) -> t
witness f = unFields $ witness_ (zkProvable @t) (\_ -> asFields $ f unit )

{-
| An in-circuit conditional. You *must* use this instead of a
| vanilla `if/then/else` expression when writing a circuit.
|
| Note that this is not a control flow operator in the usual sense.
| Both branches must return a CircuitValue, and each branch will be evaluated.
|
-}
zkIf :: forall a. CircuitValue a =>  Bool -> a -> a -> a
zkIf b t f = unFields res
  where
    res :: AsFields a
    res = zkIf_ b prover (asFields t) (asFields f)

    prover :: Provable (AsFields a)
    prover = zkProvable @a
