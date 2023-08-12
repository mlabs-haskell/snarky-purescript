module SnarkyPS.Lib.Provable where

import Prelude (Unit, unit, const, (<<<), ($))
import Prim.Row
import Prim.Row as Row
import Prim.RowList
import Data.Symbol
import Record as R
import Type.Proxy
import Partial
import Partial.Unsafe
import Data.Maybe
import Data.Array
import Data.Tuple
import Unsafe.Coerce

import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.CircuitValue.Class
import SnarkyPS.Lib.CircuitValue
import SnarkyPS.Lib.Circuit

foreign import zkIf_ :: forall t. Bool -> Provable t -> t -> t -> t

foreign import zkIfI_ :: forall t. Bool -> t -> t -> t

foreign import witness_ :: forall t. Provable t -> (Unit -> t) -> t

foreign import logAndThen :: forall t res. t -> res -> res

foreign import logAndThen_ :: forall t res. t -> res -> res

foreign import log :: forall t. t -> Unit

foreign import log_ :: forall t. t -> Unit

zkIf :: forall a. CircuitValue a =>  Bool -> a -> a -> a
zkIf b t f = fromAsFields res
  where
    a :: Proxy a
    a = Proxy

    res :: AsFields a
    res = zkIf_ b prover (asFields t) (asFields f)

    prover :: Provable (AsFields a)
    prover = zkProvable @a
