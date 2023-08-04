module SnarkyPS.Lib.Provable where

import Prelude (Unit, unit, const, (<<<))
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

import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.CircuitValue
import SnarkyPS.Lib.Circuit

foreign import zkIf_ :: forall t. Bool -> Provable t -> t -> t -> t

foreign import witness_ :: forall t. Provable t -> (Unit -> t) -> t

zkIf :: forall a. Sized a => Bool -> a -> a ->  a
zkIf b t f = zkIf_ b (provableSized @a) t f

witness :: forall a. Sized a => (Unit -> a) -> a
witness f = witness_ (provableSized @a) f
