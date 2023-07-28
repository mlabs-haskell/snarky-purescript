module SnarkyPS.Lib.CircuitValue where

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
import Data.Array ((:))
import Data.Bifunctor
import Unsafe.Coerce
import Safe.Coerce
import Record.Unsafe
import Data.Tuple
import Data.Array (null, uncons, length, take, drop)
import Simple.JSON

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.FieldClasses

foreign import  error :: forall t1 t2. t1 -> t2

{- Functions for working with opaque types, do not export  -}
foreign import sizeToInt :: SizeInFields -> Int
foreign import intToSize :: Int -> SizeInFields

withSize :: SizeInFields -> (Int -> Int) -> SizeInFields
withSize size f = intToSize $ f (sizeToInt size)


foreign import arrToFields :: Array Field -> Fields
foreign import fieldsToArr :: Fields -> Array Field

withFields :: Fields -> (Array Field -> Array Field) -> Fields
withFields fields f = arrToFields $ f (fieldsToArr fields)

{- We *could* exclude empty records but it'd make the instances even more confusing,
   so we just cheat in the case of checking the empty record and FFI in a function that
   doesn't return anything.
-}
foreign import checkEmptyRec :: forall (row :: Row Type). Record row -> Context Void

{- Hash stuff for toInput -}
foreign import emptyHash :: HashInput

foreign import appendHash :: HashInput -> HashInput -> HashInput

-- Don't export the constructor!
newtype Struct :: Row Type -> Type
newtype Struct row = Struct (Array Field)

instance Show (Struct r) where
  show (Struct r) = "Struct " <> show r


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

  toJSONL :: Proxy list -> Record row -> JSON

  fromJSONL :: Proxy list -> Proxy row -> JSON -> Maybe (Record row)

  toInputL :: Proxy list -> Record row -> HashInput

  checkL :: Proxy list -> Record row -> Context Void

  fromFieldsL :: Proxy list -> Proxy row -> Fields -> Maybe (Record row)

instance CircuitValueRL Nil () where
  toFieldsL _ _ = arrToFields []

  sizeInFieldsL _ = intToSize 0

  toJSONL _ = error "TODO 1"

  fromJSONL _ = error "TODO 2"

  toInputL _ _ = emptyHash

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

  toJSONL _ = error "TODO 3"

  fromJSONL _ = error "TODO 4"

  toInputL _ rec = appendHash thisHash restHash
    where
      thisHash = toInput arg

      restHash = toInputL listRest rowRest

      key :: Proxy label
      key = Proxy

      arg :: t
      arg = get key rec

      listRest :: Proxy listRest
      listRest = Proxy

      rowRest :: Record rowRest
      rowRest = delete key rec


  checkL _ rec = do
    let key = Proxy :: Proxy label
    assertAndThen (check (get key rec)) $ do
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

  toInput :: t -> HashInput

  toJSON :: t -> JSON

  fromJSON :: JSON -> Maybe t

  check :: t -> Context Void

  fromFields :: Proxy t -> Fields -> Maybe t

instance (
    RowToList row list
  , CircuitValueRL list row
  ) => CircuitValue (Record row) where
  toFields _ = toFieldsL (Proxy :: Proxy list)

  sizeInFields _ = sizeInFieldsL (Proxy :: Proxy list)

  toJSON = error "TODO 6"

  fromJSON = error "TODO 7"

  toInput = toInputL (Proxy :: Proxy list)

  check rec = checkL (Proxy :: Proxy list) rec

  fromFields _ = fromFieldsL (Proxy :: Proxy list) (Proxy :: Proxy row)
else instance FieldLike t => CircuitValue t where
  toFields _ t = arrToFields [toField t]

  sizeInFields _ = intToSize 1

  toJSON = error "TODO 9"

  fromJSON = error "TODO 10"

  toInput = fieldLikeToInput

  check = checkField

  fromFields _ fields = case fieldsToArr fields of
    [t] -> Just $ fromField t
    anythingElse ->  Nothing


forgetStruct :: forall (row :: Row Type). Struct row -> Array Field
forgetStruct (Struct inner) = inner

class Offset :: RowList Type -> Symbol -> Constraint
class Offset list symbol where
  getOffset :: Proxy list -> Proxy symbol -> Tuple Int Int

instance ( ListToRow (Cons l t listRest) rowFull
         , PR.Cons l t rowRest rowFull
         , CircuitValueRL listRest rowRest
         , CircuitValue t) => Offset (Cons l t listRest) l where
  getOffset _ _ = let sizeT = case sizeToInt $ sizeInFields (Proxy :: Proxy t) of
                        0 -> 0
                        n -> n - 1
                      i = 1 + sizeT + sizeToInt (sizeInFieldsL (Proxy :: Proxy listRest))
                  in Tuple i sizeT
else instance (Offset listRest  symbol) => Offset (Cons l t listRest) symbol where
  getOffset _ _ =  getOffset (Proxy :: Proxy listRest) (Proxy :: Proxy symbol)

getIndex :: forall @label row list
         . CircuitValue (Record row) =>
           RowToList row list =>
           Offset list label =>
           Record row ->
           Tuple Int Int
getIndex  _ = case offset of
  Tuple i size -> Tuple (totalSize - i) ((totalSize - i) + size)
 where
   offset = getOffset (Proxy :: Proxy list) (Proxy :: Proxy label)
   totalSize = sizeToInt $ sizeInFields (Proxy :: Proxy (Record row))

-- Dealing with impure Structs might not be worth it? The constraints would be difficult to write

testOffset :: forall (list :: RowList Type) (row :: Row Type) (symbol :: Symbol). RowToList row list => Offset list symbol => Proxy symbol -> Record row -> Tuple Int Int
testOffset _ _ = getOffset (Proxy :: Proxy list) (Proxy :: Proxy symbol)

test1 = testOffset (Proxy :: Proxy "one") myrec

type MyRec1 = (one :: U64, two :: U64, three :: U64)
type MyRec2 = (a :: U64, b :: U64, c :: {c1 :: U64, c2 :: U64}, d :: U64, e :: U64 )

myrec :: {one :: U64, two :: U64, three :: U64}
myrec = {one: u64 1, two: u64 2, three: u64 3}

myrec2 = {a: u64 1, b: u64 2, c: {c1: u64 3, c2: u64 4}, d: u64 5, e: u64 6}




{- Old, might need some of it later


  In order to construct a Pure `Struct` (pure in the SnarkyJS sense,
  i.e. records that only contain Field-like fields or other pure records),
  we have to ensure that the type we use for construction is
  "field-like all the way down".

  PureStructableRL and PureStructableArg are the mutually recursive type classes that
  implement that constraint. We need both of them because the constraint over Records
  requires the RowList class parameter & fundep.

  Note that, in principle, we should be able to derive the index of the element at each label in the
  array we get from the toFields function. That would likely be *necessary* if we deviate
  from SnarkyJS. (Hopefully this is obvious, but you'd need to derive it at the type level
  via classes w/ fundeps and a type level Nat - main reason I didn't do it is that I'm not 100% sure
  how arrays get encoded into Fields )


class PureStructableRL :: RowList Type -> Row Type -> Constraint
class PureStructableRL list row | list -> row where
  toFieldsRL :: Proxy list -> Record row -> Array Field
instance PureStructableRL Nil () where
  toFieldsRL _ _ = []
else instance (
    PureStructableArg t
  , PR.Lacks label rowRest
  , PR.Cons label t rowRest rowFull
  , RowToList rowFull (Cons label a listRest)
  , IsSymbol label
  , PureStructableRL listRest rowRest
  ) => PureStructableRL (Cons label t listRest) rowFull where
  toFieldsRL _ rec =
    let proxy :: Proxy label
        proxy = Proxy

        field :: t
        field = get proxy rec

    in  toFieldsArg field <> toFieldsRL (Proxy :: Proxy listRest) (delete proxy rec)

class PureStructableArg :: Type -> Constraint
class PureStructableArg t where
  toFieldsArg :: t -> Array Field


--instance PureStructableArg t => PureStructableArg (Array t)
instance (
    RowToList row list
  , PureStructableRL list row
  ) => PureStructableArg (Record row) where
  toFieldsArg rec = toFieldsRL  (Proxy :: Proxy list) rec
else instance FieldLike t => PureStructableArg t where
  toFieldsArg t = [toField t]

wrapObjectStr :: Array (Tuple String String) -> String
wrapObjectStr arr = if null arr then "{}" else "{ " <> wrapHelper arr
  where
    wrapLabel :: String -> String
    wrapLabel str = "\"" <> str <> "\""
    wrapHelper :: Array (Tuple String String) -> String
    wrapHelper xs = case uncons xs of
      Nothing -> ""
      Just {head: Tuple fname fval, tail: []} -> wrapLabel fname <> ": " <> fval <> " }"
      Just {head: Tuple fname fval, tail: rest} -> wrapLabel fname <> ": " <> fval <> ", " <> wrapHelper rest


-- We can't write this class directly (we need the above classes first)
-- Also, in principle we could define instances for non-Row-based SoP ADTs
-- so it's best to leave this open for now
class PureStructable :: Row Type  -> Constraint
class PureStructableArg (Record row) <= PureStructable row where
  toStruct :: Record row -> Struct row
instance PureStructableArg (Record row) => PureStructable row where
  toStruct rec = Struct $ toFieldsArg rec

class SizedRL :: RowList Type -> Constraint
class SizedRL list where
  getSizeRL :: Proxy list -> Int

instance SizedRL Nil where
  getSizeRL _ = 0
else instance (SizedArg t, SizedRL rest) => SizedRL (Cons l t rest) where
  getSizeRL _ = getSizeArg (Proxy :: Proxy t) + getSizeRL (Proxy :: Proxy rest)

class SizedArg :: Type -> Constraint
class SizedArg t where
  getSizeArg :: Proxy t -> Int

instance (
    RowToList row list
  , SizedRL list
  ) => SizedArg (Record row) where
    getSizeArg _ = getSizeRL (Proxy :: Proxy list)
else instance FieldLike t => SizedArg t where
  getSizeArg _ = 1

getSizeF :: forall list row. RowToList row list => SizedRL list => Record row -> Int
getSizeF _ = getSizeRL (Proxy :: Proxy list)

-}
