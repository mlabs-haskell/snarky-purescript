{- Module that defines helpers / utilities for working with Records and Structs

-}

module SnarkyPS.Lib.CircuitValue.Data (
    class ZkData
  , zkMorph
  , class GetField
  , getField
  , get
  , inj
  , class Sized
  , sizeInFields_
  , class SizedList
  , sizeL
  , class Locate
  , loc
  , module EXPORT
  , class FromFields
  , fromFields_
  , class ZkFromDataRow
  , class ZkDataRow
  , class ZkFromData
  , class Gettable
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
import Data.Variant (Variant)
import Data.Variant as V
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
import SnarkyPS.Lib.CircuitValue.Class (class CircuitValue, sizeInFields, toFields, check,  Struct, Enum, forgetStruct, forgetEnum) as EXPORT



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
    ZkData f g ps zk
  , PR.Cons label (f ps) rowRest rowFull
  , PR.Cons label (g zk) zkRowRest zkRowFull
  , RowToList rowFull (Cons label (f ps) listRest)
  , RowToList zkRowFull (Cons label (g zk) zkListRest)
  , IsSymbol label
  , ZkDataRow listRest rowRest zkListRest zkRowRest
  ) => ZkDataRow (Cons label (f ps) listRest) rowFull (Cons label (g zk) zkListRest) zkRowFull
else instance (
    Sized t
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
instance ZkData Enum Enum r r where
  zkMorph = identity
instance ZkData Struct Struct r r where
  zkMorph = identity

{- TODO: Figure out why we need this! I really don't understand why ZkData isn't sufficient in every case :-( -}
class ZkFromData :: (Row Type -> Type) -> (Row Type -> Type) -> Row Type  -> Row Type -> Constraint
class ZkFromData f g ps zk | ps -> zk, g -> f
instance (ZkFromDataRow psL psR zkL zkR, RowToList psR psL, RowToList zkR zkL, CircuitValue (Record psR)) => ZkFromData Record Struct psR zk
instance (ZkFromDataRow psL psR zkL zkR, RowToList psR psL, RowToList zkR zkL,  CircuitValue (Variant psR)) => ZkFromData Variant Enum psR zkR


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

instance (SizedList zkList zkRow, RowToList zkRow zkList) => Sized (Struct zkRow) where
  sizeInFields_ _ = sizeL (Proxy :: Proxy zkList) (Proxy :: Proxy zkRow)
else instance (Sized (Variant zkRow)) => Sized (Enum zkRow) where
  sizeInFields_ _ = sizeInFields_ (Proxy :: Proxy (Variant zkRow))
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

class FromFields t where
  {-| DO NOT USE THIS -}
  fromFields_ :: Proxy t -> Array Field -> t

-- super unsafe instances for Struct/Enum
instance FromFields (Enum row) where
  fromFields_ _ = Enum
else instance FromFields (Struct row) where
  fromFields_ _ = Struct
else instance CircuitValue t => FromFields t where
  fromFields_ proxy = unsafePartial fromJust <<< fromFields proxy <<< arrToFields

{- Simplified constraint for use in Prelude -}

class (Sized a, FromFields a) <= Gettable a
instance (Sized a, FromFields a) => Gettable a

{- Solely to avoid constraint noise & reduce the need for type applications & proxies -}
class GetField :: Symbol -> Type -> RowList Type -> Row Type  -> Constraint
class GetField label t list row | list -> row, label row -> t where
  getField :: Proxy label -> Proxy list -> Proxy row -> Struct row -> t

instance ( FromFields t
         , RowToList row list
         , SizedList list row
         , IsSymbol label
         , Locate list label
         , PR.Cons label t rowRest row -- Note: Need this for the compiler to infer the type of `t` (tho you'd think the fundep in GetField would suffice?)
         ) => GetField label t list row where
  getField label list row = fromFields_ (Proxy :: Proxy t) <<< slice l r <<< forgetStruct
    where
      Tuple l r = getIndex @label @list

{- This the one people should actually use -}
get :: forall @label t row list
    . GetField label t list row
    => RowToList row list
    => Struct row
    -> t
get = getField (Proxy :: Proxy label) (Proxy :: Proxy list) (Proxy :: Proxy row)

{- Create a Variant -}
inj :: forall @sym a r1 r2. PR.Cons sym a r1 r2 => IsSymbol sym => a -> V.Variant r2
inj = V.inj (Proxy :: Proxy sym)
