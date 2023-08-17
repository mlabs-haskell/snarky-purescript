module SnarkyPS.Lib.Prelude (
    module SnarkyPS.Lib.Context
  , module SnarkyPS.Lib.Bool
  , module SnarkyPS.Lib.Int
  , module SnarkyPS.Lib.Field
  , module SnarkyPS.Lib.FieldClasses
  , module SnarkyPS.Lib.Circuit
  , module SnarkyPS.Lib.Provable
  , module SnarkyPS.Lib.Hash

  , module SnarkyPS.Lib.CircuitValue
  , module SnarkyPS.Lib.CircuitValue.Match

  -- Data.Variant re-export of Variant type
  , module V

  -- Misc
  , assert
  , assertEq
  , assertM
  , get
  , set
  , over
  ) where


import Prelude (($),(<<<),void,Unit,pure)
import Data.HeytingAlgebra
import Prim.RowList (class RowToList)

import Data.Variant (Variant) as V

import SnarkyPS.Lib.Bool (assertTrue)
import SnarkyPS.Lib.Circuit (Circuit, Proof, Provable, debug, mkCircuit, prove)
import SnarkyPS.Lib.CircuitValue (class AsFieldsOf, class AsFieldsOfL, class CircuitValue, class CircuitValueEL, class CircuitValueRL, class Index, class Locate, class NonEmpty, class RowLen, class ZField, AsFields, ZEnum, ZStruct, ZkM(..), getField, get_, inj, inj_, liftFields, over_,setField, set_)
import SnarkyPS.Lib.CircuitValue.Match (class MatchE, class Matchable, class MkMatch, Match, caseOn, caseOn_, match, match_, mkMatch, switch, (==>))
import SnarkyPS.Lib.Context (Assertion, assertAndThen, assertMany)
import SnarkyPS.Lib.Field (class MakeField, Bool, DivModResult, Field, Fields, SizeInFields, field, fromBoolean, toBoolean)
import SnarkyPS.Lib.FieldClasses (class CoerceBool, class FieldLike, class ZkEq, class ZkOrd, ZUnit, bool, zUnit, zkAssertEq, (#<), (#<=), (#==), (#>), (#>=))
import SnarkyPS.Lib.Hash (Hash, hashFields, hashFields')
import SnarkyPS.Lib.Int (class CoerceU64, U64, u64)
import SnarkyPS.Lib.Provable (witness, zkIf)


{-
| Assert that two values are equivalent.
| This is a variant of the ZkEq method that
| does not require a string (which is often superfluous)
-}
assertEq :: forall a. ZkEq a => a -> a -> Assertion
assertEq = zkAssertEq ""

{-
| Assert that some value must be true. Useful for
| preliminary assertions.
-}
assert :: Bool -> ZkM Unit
assert b = assertM $ pure (assertTrue "" b)

{-
| Perform a monadic assertion in the middle of a monadic computation.
|
| This is an alias for `Control.Monad.void`
-}
assertM :: ZkM Assertion -> ZkM Unit
assertM = void

{-
| Retrieve the element at a given label from a ZStruct.
|
| This requires a type application for the label, e.g.
| `get @"fieldLabel" struct`
-}
get :: forall @l t row list
    . ZField l t list row
    => RowToList row list
    => CircuitValue t
    => ZStruct row
    -> ZkM t
get = liftFields <<<  get_ @l

{-
| Replace the value at a given label in a ZStruct
| with the argument value (which must have the same type)
|
| This requires a type application for the label, e.g.
| `set @"fieldLabel" (u64 10) struct`
-}
set :: forall @l t row list
    . ZField l t list row
    => RowToList row list
    => CircuitValue t
    => CircuitValue (ZStruct row)
    => t
    -> ZStruct row
    -> ZStruct row
set =  set_ @l

{-
| Applies a function to the value at a given label in a ZStruct,
| returning a ZStruct containing the new value at that label.
|
| This requires a type application for the label, e.g.
| `over @"fieldLabel" (\x -> x + (u64 10)) struct`
-}
over :: forall @l t row list
     . ZField l t list row
     => RowToList row list
     => CircuitValue t
     => (t -> t)
     -> ZStruct row
     -> ZStruct row
over = over_ @l
