{- Module that defines helpers / utilities for working with Records and Structs

-}

module SnarkyPS.Lib.CircuitValue.Data (
    Struct
  , forgetStruct
  , Enum
  , forgetEnum
  , class ZkData
  , zkMorph
  , class GetField
  , getField
  , get
  , class Sized
  , sizeInFields_
  , class SizedList
  , sizeL
  , class Locate
  , loc
  , module EXPORT
  , class UnsafeFromFields
  , unsafeFromFields
  , class ZkFromDataRow
  , class ZkDataRow
  ) where

import Prelude hiding (Void)

import Data.BigInt
import Data.BigInt as BI
import Type.Data.Symbol
import Type.Proxy
import Type.RowList (class RowToList, class ListToRow, class RowListNub, Cons, Nil, RowList)
import Data.Maybe
import Effect
import Record
import Prim.Row as PR
import Record as Rec
import Data.Array ((:))
import Data.Bifunctor
import Unsafe.Coerce
import Safe.Coerce
import Record.Unsafe
import Data.Tuple
import Data.Array (null, uncons, length, take, drop, length, replicate, slice)
import Simple.JSON
import Data.Variant
import Type.Data.Peano.Nat
import Partial
import Partial.Unsafe
import Data.Maybe

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.CircuitValue.Class
import SnarkyPS.Lib.CircuitValue.Class (class CircuitValue, sizeInFields, toFields, check, fromFields) as EXPORT


{-
    Core Data Abstractions & Helpers
-}

-- Don't export the constructors!

-- Record abstraction for zk circuits
newtype Struct :: Row Type -> Type
newtype Struct row = Struct (Array Field)

instance Show (Struct r) where
  show (Struct r) = "Struct " <> show r

-- Variant abstraction for zk circuits
newtype Enum :: Row Type -> Type
newtype Enum row = Enum (Array Field)

instance Show (Enum r) where
  show (Enum r) = "Enum " <> show r

forgetStruct :: forall (row :: Row Type). Struct row -> Array Field
forgetStruct (Struct inner) = inner

forgetEnum :: forall (row :: Row Type). Enum row -> Array Field
forgetEnum (Enum inner) = inner

{-
    Machinery for transforming Records/Variants into the Core Data Abstractions (Structs/Enums)
-}
class ZkDataRow :: RowList Type -> Row Type -> RowList Type -> Row Type -> Constraint
class ZkDataRow psList psRow zkList zkRow | psList -> psRow, psList -> zkList, zkList -> zkRow

instance ZkDataRow Nil () Nil ()
else instance (
    ZkDataRow rl r rl' r'
  , RowToList r rl
  , RowToList r' rl'
  , PR.Cons label (Record r) rowRest rowFull
  , PR.Cons label (Struct r') zkRowRest zkRowFull
  , RowToList rowFull (Cons label (Record r) listRest)
  , RowToList zkRowFull (Cons label (Struct r') zkListRest)
  , IsSymbol label
  , ZkDataRow listRest rowRest  zkListRest zkRowRest
  ) => ZkDataRow (Cons label (Record r) listRest) rowFull (Cons label (Struct r') zkListRest) zkRowFull
else instance (
    ZkDataRow rl r rl' r'
  , RowToList r rl
  , RowToList r' rl'
  , PR.Cons label (Variant r) rowRest rowFull
  , PR.Cons label (Enum r') zkRowRest zkRowFull
  , RowToList rowFull (Cons label (Variant r) listRest)
  , RowToList zkRowFull (Cons label (Enum r') zkListRest)
  , IsSymbol label
  , ZkDataRow listRest rowRest zkListRest zkRowRest
  ) => ZkDataRow (Cons label (Variant r) listRest) rowFull (Cons label (Enum r') zkListRest) zkRowFull
else instance (
    FieldLike t
  , PR.Cons label t rowRest rowFull
  , PR.Cons label t zkRowRest zkRowFull
  , RowToList rowFull (Cons label t listRest)
  , RowToList zkRowFull (Cons label t zkListRest)
  , IsSymbol label
  , ZkDataRow listRest rowRest zkListRest zkRowRest
  ) => ZkDataRow (Cons label t listRest) rowFull (Cons label t zkListRest) zkRowFull

class ZkFromDataRow :: RowList Type -> Row Type ->  RowList Type -> Row Type -> Constraint
class ZkDataRow psL psR zkL zkR <= ZkFromDataRow zkL zkR  psL psR  | zkL -> psL, zkL -> zkR, psL -> psR
instance ZkDataRow psL psR zkL zkR => ZkFromDataRow zkL zkR psL psR

-- For transforming Records/Variants into Structs/Enums - Type version
class ZkData :: (Row Type -> Type) -> (Row Type -> Type) -> Row Type  -> Row Type -> Constraint
class ZkData f g ps zk | zk -> ps, f -> g  where
  zkMorph :: f ps -> g zk
instance (ZkDataRow psL psR zkL zkR, RowToList psR psL, RowToList zkR zkL, CircuitValue (Record psR)) => ZkData Record Struct psR zkR where
  zkMorph = Struct <<< fieldsToArr <<< toFields (Proxy :: Proxy (Record psR))
instance (ZkDataRow psL psR zkL zkR, RowToList psR psL, RowToList zkR zkL,  CircuitValue (Variant psR)) => ZkData Variant Enum psR zkR  where
  zkMorph = Enum <<< fieldsToArr <<< toFields (Proxy :: Proxy (Variant psR))

{-
     Record  Utilities
-}

class SizedList :: RowList Type -> Row Type -> Constraint
class SizedList list row | list -> row where
  sizeL :: Proxy list -> Proxy row -> SizeInFields

instance SizedList Nil () where
  sizeL _ _ = intToSize 0
else instance (
    Sized t
  , PR.Lacks label rowRest
  , PR.Cons label t rowRest rowFull
  , RowToList rowFull (Cons label t listRest)
  , IsSymbol label
  , SizedList listRest rowRest
  ) => SizedList (Cons label t listRest) rowFull where
  sizeL _ _ = intToSize
             $ (sizeToInt $ sizeInFields_ (Proxy :: Proxy t))
               + (sizeToInt $ sizeL (Proxy :: Proxy listRest) (Proxy :: Proxy rowRest))

class Sized ::  Type -> Constraint
class Sized t where
  sizeInFields_ :: Proxy t -> SizeInFields

instance (ZkData Record Struct zkRow row, CircuitValue (Record row)) => Sized (Struct zkRow) where
  sizeInFields_ _ = sizeInFields (Proxy :: Proxy (Record row))
else instance (ZkData Variant Enum zkRow row, CircuitValue (Variant row)) => Sized (Enum zkRow) where
  sizeInFields_ _ = sizeInFields (Proxy :: Proxy (Variant row))
else instance CircuitValue t => Sized t where
  sizeInFields_ proxy = sizeInFields proxy

type Loc = {size :: Int, rest :: Int}

{- Helper typeclass to enable indexing into Structs. `loc` gives us the size of the
   element that corresponds to the label & the size of the remaining elements,
   which we use to get the [Field] slice that represents the corresponding element -}
class Locate :: RowList Type -> Symbol -> Constraint
class Locate list symbol where
  loc :: Proxy list -> Proxy symbol -> Loc

instance ( RowToList rowFull (Cons l t listRest)
         , PR.Cons l t rowRest rowFull
         , SizedList listRest rowRest
         , Sized t
         ) => Locate (Cons l t listRest) l where
  loc  _ _ = let size = sizeToInt $ sizeInFields_ (Proxy :: Proxy t)
                 rest = sizeToInt $ sizeL (Proxy :: Proxy listRest) (Proxy :: Proxy rowRest)
             in {size: size, rest: rest}
else instance (Locate listRest  symbol) => Locate (Cons l t listRest) symbol where
  loc _ _ =  loc (Proxy :: Proxy listRest) (Proxy :: Proxy symbol)

{- This gets us the indices for the slice -}
getIndex :: forall @label @list row
         . RowToList row list =>
           SizedList list row =>
           Locate list label =>
           Tuple Int Int
getIndex = Tuple l r
 where
   size = pos.size
   rest = pos.rest
   l = totalSize - (size + rest)
   r = totalSize - rest
   pos = loc (Proxy :: Proxy list) (Proxy :: Proxy label)
   totalSize = sizeToInt $ sizeL (Proxy :: Proxy list) (Proxy :: Proxy row)


{- Need this to avoid making Struct and Enum CircuitValues. They could be,
   but then we'd have to shove EVERYTHING into Class.purs.

   Super unsafe! Only for internal use!
-}

class UnsafeFromFields t where
  {-| DO NOT USE THIS -}
  unsafeFromFields :: Proxy t -> Array Field -> t

instance UnsafeFromFields (Enum row) where
  unsafeFromFields _ = Enum
else instance UnsafeFromFields (Struct row) where
  unsafeFromFields _ = Struct
else instance CircuitValue t => UnsafeFromFields t where
  unsafeFromFields proxy =  unsafePartial fromJust <<< fromFields proxy <<< arrToFields

{- Solely to avoid constraint noise & reduce the need for type applications & proxies -}
class GetField :: Symbol -> Type -> RowList Type -> Row Type  -> Constraint
class GetField label t list row | list -> row, label row -> t where
  getField :: Proxy label -> Proxy list -> Proxy row -> Struct row -> t

instance ( UnsafeFromFields t
         , RowToList row list
         , SizedList list row
         , IsSymbol label
         , Locate list label
         , PR.Cons label t rowRest row -- Note: Need this for the compiler to infer the type of `t` (tho you'd think the fundep in GetField would suffice?)
         ) => GetField label t list row where
  getField label list row = unsafeFromFields (Proxy :: Proxy t) <<< slice l r <<< forgetStruct
    where
      Tuple l r = getIndex @label @list

{- This the one people should actually use -}
get :: forall @label t row list
    . GetField label t list row
    => RowToList row list
    => Struct row
    -> t
get = getField (Proxy :: Proxy label) (Proxy :: Proxy list) (Proxy :: Proxy row)

