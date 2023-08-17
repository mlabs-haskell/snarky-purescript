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

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, identity, pure, ($), (+), (-), (<<<), (<>))

import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(Proxy))
import Type.RowList (class RowToList, Cons, RowList)
import Data.Tuple (Tuple(Tuple))
import Prim.Row (class Cons) as PR
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (length, slice)
import Data.Variant (inj, Variant) as V

import SnarkyPS.Lib.FieldClasses (ZUnit, zUnit)
import SnarkyPS.Lib.CircuitValue.Class (class CircuitValue, class CircuitValueRL, class NonEmpty, AsFields, ZStruct, forgetAsFields', forgetStruct, sizeInFields, sizeInFieldsL, sizeToInt, toFields, unFields)
import SnarkyPS.Lib.CircuitValue.Class (class AsFieldsOf, class AsFieldsOfL, class CircuitValue, class CircuitValueEL, class CircuitValueRL, class Index, class NonEmpty, class RowLen, AsFields, ZEnum, ZStruct, asFields, forgetAsFields, forgetAsFields', forgetEnum, forgetStruct, fromEnum, fromFields, fromFieldsEL, fromFieldsL, fromStruct, intToSize, sizeInFields, sizeInFieldsEL, sizeInFieldsL, sizeToInt, toEnum, toFields, toFieldsEL, toFieldsL, toStruct, unFields, unsafeHead) as EXPORT

type Loc = {size :: Int, rest :: Int}

{-
| Helper typeclass to enable indexing into Structs. `loc` gives us the size of the
| element that corresponds to the label & the size of the remaining elements,
| which we use to get the [Field] slice that represents the corresponding element
-}
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
  ) => Locate (Cons l t listRest) rowFull symbol where
  loc _ _ _ =  loc (Proxy :: Proxy listRest) (Proxy :: Proxy rowRest) (Proxy :: Proxy symbol)

{- | This gets us the indices for the slice into a ZStruct.  -}
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


{- | Solely to avoid constraint noise & reduce the need for type applications & proxies -}
class ZField :: Symbol -> Type -> RowList Type -> Row Type  -> Constraint
class ZField label t list row | list -> row, label list -> t where
  {- | Don't use this. Use `get` instead. -}
  getField :: Proxy label -> Proxy list -> Proxy row -> ZStruct row -> AsFields t

  {- | Don't use this. Use `set` instead. -}
  setField :: Proxy label -> Proxy list -> Proxy row -> t -> ZStruct row -> ZStruct row

instance ( CircuitValue t
         , RowToList row list
         , CircuitValueRL list row
         , NonEmpty list
         , IsSymbol label
         , Locate list row label
         , PR.Cons label t rowRest row -- Note: Need this for the compiler to infer the type of `t` (tho you'd think the fundep in GetField would suffice?)
         ) => ZField label t list row where
  getField _ _ _ = unsafeCoerce  <<< slice l r <<< forgetStruct
    where
      Tuple l r = getIndex @label @list

  setField _ _ _ t struct =
    let structArray = forgetStruct struct

        Tuple l r = getIndex @label @list

        tFields = forgetAsFields' $ toFields (Proxy :: Proxy t) t

        before = slice 0 l structArray

        after = slice r (length structArray) structArray

        res = before <> tFields <> after

    in unsafeCoerce res :: ZStruct row


{- The interface for Prelude should be monadic, so these generally shouldn't be used directly -}

{- | Don't use this. Use `get` instead. -}
get_ :: forall @label t row list
    . RowToList row list
    => ZField label t list row
    => ZStruct row
    -> AsFields t
get_ = getField (Proxy :: Proxy label) (Proxy :: Proxy list) (Proxy :: Proxy row)

{- | Don't use this. Use `set` instead. -}
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

{- | Don't use this. Use `over` instead. -}
over_ :: forall @label t row list
      . RowToList row list
      => ZField label t list row
      => CircuitValue t
      => (t -> t)
      -> ZStruct row
      -> ZStruct row
over_ f struct = set_ @label (f <<< unFields $ get_ @label struct) struct

{-
| Create a Data.Variant Variant at a given label, provided by a type application. E.g.:
|   `inj @"aLabel" x`
|
| This exists solely because Data.Variant does not yet offer a type application version of
| inj, and passing around a bunch of `Proxy`s is horribly ugly & confusing to
| inexperienced users.
-}
inj :: forall @sym a r1 r2. PR.Cons sym a r1 r2 => IsSymbol sym => a -> V.Variant r2
inj = V.inj (Proxy :: Proxy sym)

{-
| Create a Data.Variant Variant at a given label, specialized to the case where
| the value at that label is `ZUnit`, so that users do not have to type `zUnit`
| excessively.
-}
inj_ :: forall @sym r1 r2. PR.Cons sym ZUnit r1 r2 => IsSymbol sym => V.Variant r2
inj_ = V.inj (Proxy :: Proxy sym) zUnit

{-
|  A Codensity Monad where the `f` is specialized to `Zk`
|
|  Because `Codensity f` is a Monad for *any* `f`, we can use this to
|  implement something quite like a constrained Monad while retaining
|  the ergonomic Monad API.
|
|  This isn't very interesting and is really just a hack to get us
|  `do` notation and monadic action-sequencing.
|
Most of this was adapted from an existing PureScript Codensity implementation,
but I can't remember which one now :-(

TODO: Move to its own module
-}
newtype ZkM a = ZkM (forall b. (a -> AsFields b) -> AsFields b)

{- | "Run" a ZkM -}
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

-- Convenience function
liftFields :: forall (a :: Type). CircuitValue a => AsFields a -> ZkM a
liftFields = pure <<< unFields
