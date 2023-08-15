{- Module that defines helpers / utilities for working with Records and Structs

-}

module SnarkyPS.Lib.CircuitValue.Data (
    class ZField
  , getField
  , setField
  , get_
  , set_
  , over_
  , inj
  , inj_
  , class Locate
  , loc

  -- Monad
  , ZkM (ZkM)
  , unZkM
  , liftFields
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
class Locate :: RowList Type -> Row Type -> Symbol -> Constraint
class Locate list row symbol | list -> row  where
  loc :: Proxy list -> Proxy row -> Proxy symbol -> Loc

instance ( RowToList rowFull (Cons l t listRest)
         , PR.Cons l t rowRest rowFull
         , CircuitValueRL listRest rowRest
         , CircuitValue t
         ) => Locate (Cons l t listRest) rowFull l where
  loc  _ _ _ = let size = sizeToInt $ sizeInFields (Proxy :: Proxy t)
                   rest = sizeToInt $ sizeInFieldsL (Proxy :: Proxy listRest)
               in {size: size, rest: rest}
else instance (
    Locate listRest rowRest symbol
  -- , RowToList rowFull (Cons l t listRest)
  -- , PR.Cons l t rowRest rowFull
  ) => Locate (Cons l t listRest) rowFull symbol where
  loc _ _ _ =  loc (Proxy :: Proxy listRest) (Proxy :: Proxy rowRest) (Proxy :: Proxy symbol)

{- This gets us the indices for the slice -}
getIndex :: forall @label @list row
         . RowToList row list =>
           CircuitValueRL list row =>
           Locate list row label =>
           Tuple Int Int
getIndex = Tuple l r
 where
   size = pos.size
   rest = pos.rest
   l = totalSize - (size + rest)
   r = totalSize - rest
   pos = loc (Proxy :: Proxy list) (Proxy :: Proxy row) (Proxy :: Proxy label)
   totalSize = sizeToInt $ sizeInFieldsL (Proxy :: Proxy list)


{- Solely to avoid constraint noise & reduce the need for type applications & proxies -}
class ZField :: Symbol -> Type -> RowList Type -> Row Type  -> Constraint
class ZField label t list row | list -> row, label list -> t where
  getField :: Proxy label -> Proxy list -> Proxy row -> ZStruct row -> AsFields t

  setField :: Proxy label -> Proxy list -> Proxy row -> t -> ZStruct row -> ZStruct row

instance ( CircuitValue t
         , RowToList row list
         , CircuitValueRL list row
         , NonEmpty list
         , IsSymbol label
         , Locate list row label
         , PR.Cons label t rowRest row -- Note: Need this for the compiler to infer the type of `t` (tho you'd think the fundep in GetField would suffice?)
         ) => ZField label t list row where
  getField label list row = unsafeCoerce  <<< slice l r <<< forgetStruct
    where
      Tuple l r = getIndex @label @list

  setField label list row t struct =
    let structArray = forgetStruct struct

        Tuple l r = getIndex @label @list

        tFields = forgetAsFields' $ toFields (Proxy :: Proxy t) t

        before = slice 0 l structArray

        after = slice r (length structArray) structArray

        res = before <> tFields <> after

    in unsafeCoerce res :: ZStruct row


{- The interface for Prelude should be monadic, so these generally shouldn't be used directly -}
get_ :: forall @label t row list
    . RowToList row list
    => ZField label t list row
    => ZStruct row
    -> AsFields t
get_ = getField (Proxy :: Proxy label) (Proxy :: Proxy list) (Proxy :: Proxy row)

set_ :: forall @label t row list
    . RowToList row list
    => ZField label t list row
    => t
    -> ZStruct row
    -> ZStruct row
set_ = setField label list row
  where
    label = Proxy :: (Proxy label)
    list  = Proxy :: (Proxy list)
    row   = Proxy :: (Proxy row)

over_ :: forall @label t row list
      . RowToList row list
      => ZField label t list row
      => CircuitValue t
      => (t -> t)
      -> ZStruct row
      -> ZStruct row
over_ f struct = set_ @label (f <<< unFields $ get_ @label struct) struct

{- Create a Variant -}
inj :: forall @sym a r1 r2. PR.Cons sym a r1 r2 => IsSymbol sym => a -> V.Variant r2
inj = V.inj (Proxy :: Proxy sym)

inj_ :: forall @sym r1 r2. PR.Cons sym ZUnit r1 r2 => IsSymbol sym => V.Variant r2
inj_ = V.inj (Proxy :: Proxy sym) zUnit

{-
   A Codensity Monad where the `f` is specialized to `Zk`

   Because `Codensity f` is a Monad for *any* f, we can use this to
   implement something quite like a "Constrained Monad".

   TODO: Move to its own module
-}
newtype ZkM a = ZkM (forall b. (a -> AsFields b) -> AsFields b)

unZkM :: forall a b. ZkM a -> (a -> AsFields b) -> AsFields b
unZkM (ZkM f) = f

instance Functor ZkM where
  map f (ZkM g) = ZkM (\k -> g (k <<< f))

instance Apply ZkM where
  apply (ZkM f) (ZkM g) = ZkM (\k -> f (\l -> g (k <<< l)))

instance Applicative ZkM where
  pure x = ZkM (_ $ x)

instance Bind ZkM where
  bind (ZkM f) k = ZkM (\g -> (f (\x -> unZkM (k x) g)))

instance Monad ZkM

runZkM :: forall a. ZkM (AsFields a) -> AsFields a
runZkM zk = unZkM zk identity

liftFields :: forall (a :: Type). CircuitValue a => AsFields a -> ZkM a
liftFields = pure <<< unFields
