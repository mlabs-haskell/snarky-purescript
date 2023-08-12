{- Module that defines the web of mutually recursive CircuitValue classes

-}

module SnarkyPS.Lib.CircuitValue.Class where

import Prelude hiding (Void)

import Data.BigInt
import Data.BigInt as BI
import Data.Semiring
import Data.Ring
import Data.CommutativeRing
import Prim.TypeError

import Type.Data.Symbol
import Type.Proxy
import Type.RowList (class RowToList, class ListToRow, class RowListNub, Cons, Nil, RowList)
import Data.Maybe
import Effect
import Record
import Prim.Row as PR
import Data.Array ((:))
import Data.Bifunctor
import Unsafe.Coerce
import Safe.Coerce
import Record.Unsafe
import Data.Tuple
import Data.Array (null, uncons, length, take, drop, replicate, (!!), reverse)
import Simple.JSON
import Data.Variant
import Type.Data.Peano.Nat

import Partial
import Partial.Unsafe
import Unsafe.Coerce
import Data.Maybe

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.FieldClasses

import Debug

{- NOTE: *** IMPORTANT ***

  ZStruct and ZEnum really should not have CircuitValue instances! Rearrange typeclasses to fix that


-}

{- Debug utils -}

traceMsg :: forall a b. DebugWarning => String -> a -> b -> b
traceMsg msg toTrace b = trace (msg <> "\n")
                         $ \_ -> trace toTrace
                         $ \_ -> trace "\n"
                         $ \_ -> b


{- Foreign imports -}

foreign import checkEmptyRec :: forall (row :: Row Type). Record row -> Assertion

foreign import checkEmptyVar :: forall (row :: Row Type). Variant row -> Assertion

class NonEmpty :: RowList Type -> Constraint
class NonEmpty list

instance NonEmpty (Cons l t res)
else instance (
  Fail (Text "Circuit Enums & Structs must not be empty!")
 ) => NonEmpty Nil


{-

The core of this module consists in the CircuitValueRL and CircuitValue typeclasses.

CircuitValueRL is a constraint over rows, and tells us that a record parameterized by the row
can be constructed from elements that are transformable into [Field].

CircuitValue is a constraint over *types*. Instances of CircuitValue can be represented by
[Field] as well, and are suitable for proving.

These classes are mutually recursive (indeed they have to be).

This could be implemented with TypeLevel Nats, which would save us from needing
the auxilliary `Offset` type class - but I did something similar in CTL and
typelevel math DRAMATICALLY slows down compilation (i.e. it adds MINUTES for
not-all-that-complex types)

-}
class CircuitValueRL :: RowList Type -> Row Type -> Constraint
class CircuitValueRL list row | list -> row where
  toFieldsL :: Proxy list -> Record row -> Fields

  sizeInFieldsL :: Proxy list -> SizeInFields

  checkL :: Proxy list -> Record row -> Assertion

  -- This should be
  -- Proxy list -> Proxy row -> Record row -> AsFields (Record row) -> Record row
  -- Same for toFieldsL & all the other classes -__
  -- B/c we don't use these to construct a SnarkyJS `Provable` anymore,
  -- the class logic is messed up and can be fixed by making that change.
  -- & then we can get rid of the stupid toZk/fromZk/etc functions that are super annoying
  fromFieldsL :: Proxy list -> Proxy row -> Fields -> Maybe (Record row)

instance CircuitValueRL Nil () where
  toFieldsL _ _ = arrToFields []

  sizeInFieldsL _ = intToSize 0

  checkL _  = checkEmptyRec

  fromFieldsL _ _ fields = case fieldsToArr fields of
    [] -> Just {}
    anythingElse -> Nothing

else instance (
    CircuitValue t
  , PR.Lacks label rowRest
  , PR.Cons label t rowRest rowFull
  , RowToList rowFull (Cons label t listRest)
  , IsSymbol label
  , CircuitValueRL listRest rowRest
  ) => CircuitValueRL (Cons label t listRest) rowFull where
  toFieldsL _ rec = arrToFields $ fieldsT <> fieldsRest
    where
      fieldsT = fieldsToArr $ toFields (Proxy :: Proxy t) arg

      fieldsRest = fieldsToArr $ toFieldsL (Proxy :: Proxy listRest) rowRest

      key :: Proxy label
      key = Proxy

      arg :: t
      arg = get key rec

      rowRest :: Record rowRest
      rowRest = delete key rec

  sizeInFieldsL _ =
    let sizeT = sizeToInt $ sizeInFields (Proxy :: Proxy t)
        sizeRest =  sizeToInt $ sizeInFieldsL (Proxy :: Proxy listRest)
    in intToSize (sizeT + sizeRest)

  checkL _ rec =
    let key = Proxy :: Proxy label
    in assertAndThen (check (get key rec)) $
         checkL (Proxy :: Proxy listRest) (delete key rec)

  fromFieldsL _ _ fields = case length arr >= sizeT of
    true -> case fromFieldsRest of
      Nothing -> Nothing
      Just recRest -> case fromFields proxyT (arrToFields $ take sizeT arr) of
       Nothing -> Nothing
       Just t  ->  Just $ insert label t recRest
    false -> Nothing
   where
     label  = Proxy :: Proxy label
     proxyT = Proxy :: Proxy t
     listRest = Proxy :: Proxy listRest
     rowRest  = Proxy :: Proxy rowRest
     arr   = fieldsToArr fields
     sizeT = sizeToInt $ sizeInFields proxyT
     fromFieldsRest = fromFieldsL listRest rowRest  (arrToFields $ drop sizeT arr)

class CircuitValue :: Type -> Constraint
class CircuitValue t where
  sizeInFields :: Proxy t -> SizeInFields

  toFields :: Proxy t -> t -> Fields

  check :: t -> Assertion

  fromFields :: Proxy t -> Fields -> Maybe t

-- ZStruct
instance (
    RowToList row list
  , CircuitValueRL list row
  , NonEmpty list
  ) => CircuitValue (Record row) where
  toFields _ = toFieldsL (Proxy :: Proxy list)

  sizeInFields _ = sizeInFieldsL (Proxy :: Proxy list)

  check rec = checkL (Proxy :: Proxy list) rec

  fromFields _ = fromFieldsL (Proxy :: Proxy list) (Proxy :: Proxy row)
-- Variant Instance
else instance (
    RowToList row list
  , CircuitValueEL list row
  , NonEmpty list
  ) => CircuitValue (Variant row) where
  toFields _ = toFieldsEL (Proxy :: Proxy list)

  sizeInFields _ = sizeInFieldsEL (Proxy :: Proxy list)

  check = checkEL (Proxy :: Proxy list)

  fromFields _ = fromFieldsEL (Proxy :: Proxy list) (Proxy :: Proxy row)
else instance (
    CircuitValueRL list row
  , RowToList row list
  , NonEmpty list
  ) => CircuitValue (ZStruct row) where
  toFields _ = arrToFields <<< forgetStruct

  sizeInFields _ = sizeInFieldsL (Proxy :: Proxy list)

  -- TODO
  check struct = assertTrue "" (bool true)

  -- FIXME: This is bad!
  fromFields _ = Just <<< unsafeCoerce
    where
      list = Proxy :: Proxy list
      row  = Proxy :: Proxy row
-- ZEnum

else instance (
    RowToList row list
  , CircuitValueEL list row
  , NonEmpty list
  ) => CircuitValue (ZEnum row) where
  toFields _ = arrToFields <<< forgetEnum

  sizeInFields _ = sizeInFieldsEL (Proxy :: Proxy list)

  check enum = assertTrue "" (bool true)

  -- FIXME: This is bad!
  fromFields _  = Just <<< unsafeCoerce
    where
      coerce :: Fields -> ZEnum row
      coerce = unsafeCoerce
      list = Proxy :: Proxy list
      row  = Proxy :: Proxy row
-- Bare Fieldlike instance
else instance FieldLike t => CircuitValue t where
  toFields _ t = arrToFields [toField t]

  sizeInFields _ = intToSize 1

  check = checkField

  fromFields _ fields = case fieldsToArr fields of
    [t] -> Just $ fromField t
    anythingElse ->  Nothing


{- CircuitEnum, for representing Sums/Variants/Enumerations -}

-- Helpers (we can't avoid type math here)

class RowLen :: RowList Type -> Row Type -> Nat -> Constraint
class IsNat len <= RowLen list row len | list -> len, list -> row

instance RowLen Nil () Z
else instance (
    RowLen rest rowRest n
  , RowToList row (Cons l t rest)
  , PR.Cons l t rowRest row
  , IsSymbol l) => RowLen (Cons l t rest) row (Succ n)


class Index :: Symbol -> RowList Type -> Row Type -> Nat -> Constraint
class IsNat ix <= Index label list row ix | label list -> ix, list -> row

instance ( IsNat n
         , RowLen rest rowRest n
         , RowToList row (Cons l t rest)
         , PR.Cons l t rowRest row
         , IsSymbol l
         ) => Index l (Cons l t rest) row (Succ n)

{-
     Enums
-}
class CircuitValueEL :: RowList Type -> Row Type -> Constraint
class CircuitValueEL list row | list -> row  where
  toFieldsEL :: Proxy list -> Variant row -> Fields

  sizeInFieldsEL :: Proxy list -> SizeInFields

  checkEL :: Proxy list -> Variant row -> Assertion

  fromFieldsEL :: Proxy list -> Proxy row -> Fields -> Maybe (Variant row)
{- I'll forget this if I don't write it down

   The encoding we use for Variants is:
        let tFields :: Array Field = toFields of the injected type
            vMax    :: Int = of the *largest* element in the variant (+1?)
            tDiff   :: Int = length tFields - vMax
            index   :: Distance from Nil (we index backwards for easiness, hoep that works...)
            padding :: replicate tDiff (Field 0)

        toFieldsV for a variant at a given label
          = index : (tFields <> padding)

-}

-- Technically this shouldn't be an instance, we'll need to exclude empty variants and records using another
-- type class b/c we need a base case and it's a LOT more annoying if the base case is `Cons l t Nil`
-- (I can't figure out how to make PS unify the r in `RowToList r Nil` or `ListToRow Nil r` with (), tho it'd work in Haskell)
instance CircuitValueEL Nil () where
  toFieldsEL _ _ = arrToFields []

  sizeInFieldsEL _ = intToSize 0

  checkEL _ = checkEmptyVar

  fromFieldsEL _ _ fields = Nothing
else instance (
    CircuitValue t
  , PR.Lacks label rowRest
  , PR.Cons label t rowRest rowFull
  , RowToList rowFull (Cons label t listRest)
  , CircuitValueEL listRest rowRest
  , IsSymbol label
  , PR.Union rowRest x rowFull
  , Index label (Cons label t listRest) rowFull ix
  , IsNat ix -- superfluous?
  ) => CircuitValueEL (Cons label t listRest) rowFull where
    toFieldsEL list var =  on (Proxy :: Proxy label) goThis goRest var
      where
        ix = reflectNat (Proxy :: Proxy ix)

        maxSize = sizeToInt $ sizeInFieldsEL (Proxy :: Proxy listRest) -- This should INCLUDE the constr index prefix

        pad :: Array Field -> Array Field
        pad arr =  field ix : (arr <> replicate (maxSize - len) (field 0))
          where
            len = length arr + 1 -- I think?

        goThis :: t -> Fields
        goThis t = arrToFields $ pad fieldsT
          where
            fieldsT :: Array Field
            fieldsT = fieldsToArr $ toFields (Proxy :: Proxy t) t

        --goRest :: Variant rowRest -> Fields
        goRest rest = toFieldsEL (Proxy :: Proxy listRest) rest

    sizeInFieldsEL list =  intToSize $ (max (sizeThis + 1) sizeRest) -- +1 for the Index prefix
      where
        sizeThis = sizeToInt $ sizeInFields (Proxy :: Proxy t)
        sizeRest = sizeToInt $ sizeInFieldsEL (Proxy :: Proxy listRest)

    checkEL list var = on (Proxy :: Proxy label) goThis goRest var
      where
        goThis :: t -> Assertion
        goThis = check

        --goRest :: Variant rowRest -> Assertion
        goRest = checkEL (Proxy :: Proxy listRest)

    fromFieldsEL list row fields = case uncons (fieldsToArr fields) of
       Just {head: ix, tail: rest} -> if toBigIntField ix == ix'
         then inj label <$> fromFields proxyT (arrToFields $ take sizeT rest)
         else expand <$> fromFieldsEL listRest rowRest fields
       Nothing -> Nothing
      where
        label = Proxy :: Proxy label
        listRest = Proxy :: Proxy listRest
        rowRest = Proxy :: Proxy rowRest
        proxyT = Proxy :: Proxy t
        sizeT = sizeToInt $ sizeInFields proxyT
        ix' = BI.fromInt (reflectNat (Proxy :: Proxy ix))


{- Misc helpers. Don't expose these in the Prelude -}

sizeToInt :: SizeInFields -> Int
sizeToInt = unsafeCoerce

intToSize :: Int -> SizeInFields
intToSize = unsafeCoerce

arrToFields :: Array Field -> Fields
arrToFields = unsafeCoerce

fieldsToArr :: Fields -> Array Field
fieldsToArr = unsafeCoerce

{- AsFields Utilities, should be in Field.purs but here for staging/module hierarchy reasons -}

{- This is how we represent an `Array Field` which *we* know corresponds to a particular circuit type

  I really wanted to avoid this, but I don't think we ultimately can. Type inference w/ the Struct/Enum stuff is too
  fragile.

  Ideally this would be a Functor/Applicative/Monad, but it can't satisfy the laws. It could be a *constrained* monad,
  but afaict there isn't a nice library for those in PS and since we don't have RebindableSyntax it'd be ugly as hell.
-}
foreign import data AsFields :: Type -> Type

forgetAsFields :: forall t. AsFields t -> Fields
forgetAsFields = unsafeCoerce

forgetAsFields' :: forall t. AsFields t -> Array Field
forgetAsFields' = unsafeCoerce

withAsFields :: forall t r. AsFields t -> (Fields -> r) -> r
withAsFields zk f = f (forgetAsFields zk)

asAsFields :: forall t r. CircuitValue t => t -> (AsFields t -> r) -> r
asAsFields t f = f (asFields t)

unAsFields :: forall t r. CircuitValue t => AsFields t -> (t -> r) -> r
unAsFields zt f = f (fromAsFields zt)

fromAsFields :: forall (t :: Type). CircuitValue t => AsFields t -> t
fromAsFields af = withAsFields af $ \arr -> unsafePartial fromJust (fromFields (Proxy :: Proxy t) arr)

asFields :: forall (t :: Type). CircuitValue t => t -> AsFields t
asFields t =  unsafeCoerce $ toFields (Proxy :: Proxy t) t

-- type synonyms for convenience

foreign import data ZStruct :: Row Type -> Type

forgetStruct :: forall (r :: Row Type). ZStruct r -> Array Field
forgetStruct = unsafeCoerce

toStruct :: forall (r :: Row Type) (r' :: Row Type)
         . CircuitValue (Record r)
         => AsFieldsOf (Record r) (ZStruct r')
         => Record r
         -> ZStruct r'
toStruct = unsafeCoerce <<< toFields (Proxy :: Proxy (Record r))

-- | FOR INTERNAL USE ONLY
fromStruct :: forall (r :: Row Type) (r' :: Row Type)
           . CircuitValue (Record r)
           => AsFieldsOf (Record r) (ZStruct r')
           => ZStruct r'
           -> Record r
fromStruct  = unsafeFromFields @(Record r) <<< forgetStruct

unsafeFromFields :: forall (@t :: Type). CircuitValue t => Array Field -> t
unsafeFromFields = unsafePartial fromJust <<<  fromFields (Proxy :: Proxy t) <<< arrToFields

foreign import data ZEnum :: Row Type -> Type

forgetEnum :: forall (r :: Row Type). ZEnum r -> Array Field
forgetEnum = unsafeCoerce

toEnum :: forall (r :: Row Type) (r' :: Row Type)
       . CircuitValue (Variant r)
       => AsFieldsOf (Variant r) (ZEnum r')
       => Variant r
       -> ZEnum r'
toEnum = unsafeCoerce <<< toFields (Proxy :: Proxy (Variant r))

-- | FOR INTERNAL USE ONLY
fromEnum :: forall (r :: Row Type) (r' :: Row Type)
         . CircuitValue (Variant r)
         => AsFieldsOf (Variant r) (ZEnum r')
         => ZEnum r'
         -> Variant r
fromEnum = unsafeFromFields @(Variant r) <<< forgetEnum

instance (ZkEq t, CircuitValue t) => ZkEq (AsFields t) where
  zkEq z1 z2 = zkEq (fromAsFields z1) (fromAsFields z2)
  zkAssertEq msg z1 z2 = zkAssertEq msg (fromAsFields z1) (fromAsFields z2)

instance (ZkOrd t, CircuitValue t) => ZkOrd (AsFields t) where
  zkLT b1 b2 =  (fromAsFields b1) #<  (fromAsFields b2)
  assertLT msg b1 b2 = assertLT  msg (fromAsFields b1) (fromAsFields b2)
  zkLTE b1 b2 =  (fromAsFields b1) #<=  (fromAsFields b2)
  assertLTE msg b1 b2 = assertLTE msg (fromAsFields b1) (fromAsFields b2)
  zkGT b1 b2 = (fromAsFields b1) #> (fromAsFields b2)
  assertGT msg b1 b2 = assertGT msg (fromAsFields b1) (fromAsFields b2)
  zkGTE b1 b2 = (fromAsFields b1) #>= (fromAsFields b2)
  assertGTE msg b1 b2 = assertGTE msg (fromAsFields b1) (fromAsFields b2)

-- helper class TODO: document what it's for

-- don't need methods here, we're just going to unsafeCoerce in the arg class
class AsFieldsOfL :: RowList Type -> Row Type -> RowList Type -> Row Type -> Constraint
class AsFieldsOfL list row zList zRow | list -> row, zList -> zRow, list -> zList, row -> zRow

instance AsFieldsOfL Nil () Nil ()
else instance (
    AsFieldsOfL lRest rRest zlRest zrRest
  , AsFieldsOf a zA
  , RowToList rRest lRest
  , RowToList zrRest zlRest
  , PR.Cons lbl a rRest rFull
  , PR.Cons lbl zA zrRest zrFull
  , IsSymbol lbl
  ) => AsFieldsOfL (Cons lbl a lRest) rFull (Cons lbl zA zlRest) zrFull

class AsFieldsOf :: Type -> Type -> Constraint
class AsFieldsOf a za | a -> za, za -> a

instance (
    AsFieldsOfL list row zList zRow
  , RowToList row list
  , RowToList zRow zList
  , CircuitValue (Record row)
  ) => AsFieldsOf (Record row) (ZStruct zRow)
else instance (
    AsFieldsOfL list row zList zRow
  , RowToList row list
  , RowToList zRow zList
  , CircuitValue (Variant row)
  ) => AsFieldsOf (Variant row) (ZEnum zRow)
else instance (
    CircuitValue a
  ) => AsFieldsOf a a -- maybe AsFieldsOf a (AsFields a)?

coerceFromAsFields :: forall a' a. AsFieldsOf a a' => AsFields a' -> AsFields a
coerceFromAsFields = unsafeCoerce

coerceToAsFields :: forall a a'. AsFieldsOf a a' => AsFields a -> AsFields a'
coerceToAsFields = unsafeCoerce


-- convenience for instances

zkBin :: forall t r. CircuitValue t => CircuitValue r => AsFields t -> AsFields t -> (t -> t -> r) -> AsFields r
zkBin z1 z2 f = asFields $ f (fromAsFields z1) (fromAsFields z2)

instance (Semiring t, CircuitValue t) => Semiring (AsFields t) where
  add a b = zkBin a b add
  zero    = asFields zero
  mul a b = zkBin a b mul
  one     = asFields one

instance (Ring t, CircuitValue t) => Ring (AsFields t) where
  sub a b = zkBin a b sub

instance (CommutativeRing t, CircuitValue t) => CommutativeRing (AsFields t)
