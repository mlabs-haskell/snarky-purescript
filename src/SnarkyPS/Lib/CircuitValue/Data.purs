{- Module that defines helpers / utilities for working with Records and Structs

-}

module SnarkyPS.Lib.CircuitValue.Data (
    class GetField
  , getField
  , get
  , inj
  , class Locate
  , loc
  , class IsZkL
  , class IsZk
  , coerceFromZk
  , coerceToZk
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

type Loc = {size :: Int, rest :: Int}

{- Helper typeclass to enable indexing into Structs. `loc` gives us the size of the
   element that corresponds to the label & the size of the remaining elements,
   which we use to get the [Field] slice that represents the corresponding element -}
class Locate :: RowList Type -> Symbol -> Constraint
class Locate list symbol where
  loc :: Proxy list -> Proxy symbol -> Loc

instance ( RowToList rowFull (Cons l t listRest)
         , PR.Cons l t rowRest rowFull
         , CircuitValueRL listRest rowRest
         , CircuitValue t
         ) => Locate (Cons l t listRest) l where
  loc  _ _ = let size = sizeToInt $ sizeInFields (Proxy :: Proxy t)
                 rest = sizeToInt $ sizeInFieldsL (Proxy :: Proxy listRest)
             in {size: size, rest: rest}
else instance (Locate listRest  symbol) => Locate (Cons l t listRest) symbol where
  loc _ _ =  loc (Proxy :: Proxy listRest) (Proxy :: Proxy symbol)

{- This gets us the indices for the slice -}
getIndex :: forall @label @list row
         . RowToList row list =>
           CircuitValueRL list row =>
           Locate list label =>
           Tuple Int Int
getIndex = Tuple l r
 where
   size = pos.size
   rest = pos.rest
   l = totalSize - (size + rest)
   r = totalSize - rest
   pos = loc (Proxy :: Proxy list) (Proxy :: Proxy label)
   totalSize = sizeToInt $ sizeInFieldsL (Proxy :: Proxy list)

-- don't need methods here, we're just going to unsafeCoerce in the arg class
class IsZkL :: RowList Type -> Row Type -> RowList Type -> Row Type -> Constraint
class IsZkL list row zList zRow | list -> row, zList -> zRow, list -> zList, row -> zRow

instance IsZkL Nil () Nil ()
else instance (
    IsZkL lRest rRest zlRest zrRest
  , IsZk a zA
  , RowToList rRest lRest
  , RowToList zrRest zlRest
  , PR.Cons lbl a rRest rFull
  , PR.Cons lbl zA zrRest zrFull
  , IsSymbol lbl
  ) => IsZkL (Cons lbl a lRest) rFull (Cons lbl zA zlRest) zrFull

class IsZk :: Type -> Type -> Constraint
class IsZk a za | a -> za, za -> a

instance (
    IsZkL list row zList zRow
  , RowToList row list
  , RowToList zRow zList
  , CircuitValue (Record row)
  ) => IsZk (Record row) (Record zRow)
else instance (
    IsZkL list row zList zRow
  , RowToList row list
  , RowToList zRow zList
  , CircuitValue (Variant row)
  ) => IsZk (Variant row) (Variant zRow)
else instance (
    CircuitValue a
  ) => IsZk a a

coerceFromZk :: forall a' a. IsZk a a' => Zk a' -> Zk a
coerceFromZk = unsafeCoerce

coerceToZk :: forall a a'. IsZk a a' => Zk a -> Zk a'
coerceToZk = unsafeCoerce



{- Solely to avoid constraint noise & reduce the need for type applications & proxies -}
class GetField :: Symbol -> Type -> Type -> RowList Type -> Row Type  -> Constraint
class GetField label t t' list row | list -> row, label row -> t, t -> t' where
  getField :: Proxy label -> Proxy list -> Proxy row -> Proxy t -> ZkStruct row ->  Zk t'

instance ( CircuitValue t
         , IsZk t t'
         , RowToList row list
         , CircuitValueRL list row
         , IsSymbol label
         , Locate list label
         , PR.Cons label t rowRest row -- Note: Need this for the compiler to infer the type of `t` (tho you'd think the fundep in GetField would suffice?)
         ) => GetField label t t' list row where
  getField label list row t = unsafeZk @t'  <<< slice l r <<< forgetZk'
    where
      unsafeZk :: forall (@t :: Type). Array Field -> Zk t'
      unsafeZk = unsafeCoerce

      Tuple l r = getIndex @label @list

{- This the one people should actually use -}
get :: forall @label t t' row list
    . GetField label t t' list row
    => RowToList row list
    => IsZk t t' -- just to be extra sure the compiler does its job -_-
    => ZkStruct row
    -> Zk t'
get = getField (Proxy :: Proxy label) (Proxy :: Proxy list) (Proxy :: Proxy row) (Proxy :: Proxy t)

{- Create a Variant -}
inj :: forall @sym a r1 r2. PR.Cons sym a r1 r2 => IsSymbol sym => a -> V.Variant r2
inj = V.inj (Proxy :: Proxy sym)
