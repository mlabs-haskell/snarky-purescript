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
  , module SnarkyPS.Lib.CircuitValue.Data
  , module SnarkyPS.Lib.CircuitValue.Class

  -- Misc
  , assertEq
  , assertM
  , module V
  , get
  , set
  , over
  , input
  ) where


import Prelude
import Data.HeytingAlgebra
import Prim.Row as PR
import Prim.RowList

import Unsafe.Coerce

import Data.Variant (Variant) as V

import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Circuit
import SnarkyPS.Lib.Provable
import SnarkyPS.Lib.Hash

import SnarkyPS.Lib.CircuitValue.Match
import SnarkyPS.Lib.CircuitValue
import SnarkyPS.Lib.CircuitValue.Data
import SnarkyPS.Lib.CircuitValue.Class

assertEq :: forall a. ZkEq a => a -> a -> Assertion
assertEq = zkAssertEq ""

assertM :: ZkM Assertion -> ZkM Unit
assertM = void

zkStruct :: forall (row :: Row Type). ZStruct row -> AsFields (ZStruct row)
zkStruct = unsafeCoerce

get :: forall @l t row list
    . ZField l t list row
    => RowToList row list
    => CircuitValue t
    => ZStruct row
    -> ZkM t
get = pure <<< get_ @l

set :: forall @l t row list
    . ZField l t list row
    => RowToList row list
    => CircuitValue t
    => CircuitValue (ZStruct row)
    => t
    -> ZStruct row
    -> ZStruct row
set =  set_ @l

over :: forall @l t row list
     . ZField l t list row
     => RowToList row list
     => CircuitValue t
     => (t -> t)
     -> ZStruct row
     -> ZStruct row
over = over_ @l

input :: forall (t :: Type) (t' :: Type). CircuitValue t => AsFieldsOf t t' => t -> ZkM t'
input t = unsafeCoerce t'
 where
   t' :: ZkM t
   t' = lift $ asFields t
