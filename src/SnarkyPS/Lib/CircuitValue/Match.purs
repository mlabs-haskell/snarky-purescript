module SnarkyPS.Lib.CircuitValue.Match where

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
import SnarkyPS.Lib.CircuitValue.Data
import SnarkyPS.Lib.Provable
import SnarkyPS.Lib.Hash

{- IMPORTANT! NOTE:

   We don't do `hashFields []` because `hashFields [] == hashFields [field 0] == hashFields [toField $ bool false]`
   which is, I hope, obviously bad.

   TODO: Strengthen this, perhaps by adding a salt parameter that is user chosen.
         As it stands, this is potentially vulnerable to a hash collision attack.
         We can't remove that vulnerability entirely (nature of hash functions), but
         requiring the user to provide a salt value makes it a *lot* harder. (ATM attacker just
         needs to find some value that hashes to 0.)

-}
emptyHash :: Hash
emptyHash = fieldToHash (field 0)

-- It's awkward to make Structs and Enums CircuitValue instances, here we need the `toFields` part of CV
class ToFields (t :: Type) where
  toFields_ :: t -> Array Field

instance ToFields (Struct r) where
  toFields_ = forgetStruct
else instance ToFields (Enum r) where
  toFields_ = forgetEnum
else instance CircuitValue t => ToFields t where
  toFields_ = unsafeCoerce <<< toFields (Proxy :: Proxy t)

hashCV :: forall t. ToFields t => t -> Hash
hashCV = hashFields <<< toFields_

foreign import unsafeHead :: forall (t :: Type). Array t -> t

class MatchE :: RowList Type -> Row Type -> RowList Type -> Row Type -> Type -> Constraint
class MatchE pList pRow zList zRow result | pList -> pRow, zList -> zRow  where
  -- we need to pass in a default `result` value to make this work
  caseOn_ :: result -> Proxy pList -> Proxy zList -> Record pRow -> Enum zRow -> result

instance (
    IsSymbol l
  , Matchable a b res
  , RowToList r (Cons l b Nil)
  , RowToList r' (Cons l a Nil)
  , PR.Cons l b () r
  , PR.Cons l a () r'
  , Sized res
  , CircuitValue res
  ) => MatchE (Cons l b Nil) r (Cons l a Nil) r' res where
  caseOn_ res pList zList pRec zEnum =
    let label :: Proxy l
        label = Proxy

        matcher :: b
        matcher = Rec.get label pRec

        aFields = drop 1 $ forgetEnum zEnum

        aIndex = unsafeHead aFields

        aHash = hashFields aFields
    in zkIf (aIndex #== field 1)  -- We know the index here has to be 1 b/c we're out of Variant to traverse
         (runMatch res aFields matcher )
         (res)
else instance (
    IsSymbol l
  , Matchable a b res
  , RowToList pRowFull (Cons l b  pListRest)
  , RowToList zRowFull (Cons l a zListRest)
  , PR.Lacks l pRowRest
  , PR.Cons l b pRowRest pRowFull
  , PR.Cons l a zRowRest zRowFull
  , MatchE pListRest pRowRest zListRest zRowRest res
  , Index l (Cons l a zListRest) zRowFull ix
  , IsNat ix
  , Sized res
  , CircuitValue res
  ) => MatchE (Cons l b pListRest) pRowFull (Cons l a zListRest) zRowFull res where
  caseOn_ res pList zList pRec zEnum =
    zkIf (i #== ix)
      (runMatch res valFields b)
      (g (unsafeCoerce zEnum :: Enum zRowRest))
   where
     l :: Proxy l
     l = Proxy

     b :: b
     b = Rec.get l pRec

     fields = forgetEnum zEnum
 
     i = unsafeHead fields
     ix = field <<< BI.fromInt $ reflectNat (Proxy :: Proxy ix)

     valFields = drop 1 fields

     g = caseOn_ res pListRest zListRest (Rec.delete l pRec)

     pListRest = Proxy :: Proxy pListRest
     zListRest = Proxy :: Proxy zListRest

caseOn :: forall t m r. Sized r => Matchable t m r => ToFields t => r -> t -> m -> r
caseOn res t m = runMatch res (unsafeCoerce (toFields_ t) :: Array Field)  m

data Match t r = Match t r

class Matchable :: Type -> Type -> Type -> Constraint
class Matchable (t :: Type) (m :: Type) (res :: Type) | m -> t where
  runMatch :: res -> Array Field  -> m -> res

instance (
      MatchE pList pRow zList zRow result
    , RowToList pRow pList
    , RowToList zRow zList
    , Sized result
    , CircuitValue result
    ) => Matchable (Enum zRow) (Record pRow) result where
  runMatch res e r = caseOn_
                       res
                       (Proxy :: Proxy pList)
                       (Proxy :: Proxy zList)
                       r
                       (unsafeCoerce e :: Enum zRow)

instance (
      ZkFromData Record Struct r zR
    , ToFields (Record r)
    , Sized res
    , CircuitValue res
    ) => Matchable (Struct zR) (Array (Match (Record r) res)) res where
  runMatch def rFields arr = case uncons arr of
    Nothing -> def
    Just {head: Match r' res, tail: t} ->
      zkIf (hashFields rFields #== hashCV r')
        (res)
        (runMatch def rFields t)

instance (Sized r, CircuitValue r) => Matchable Bool (Array (Match Boolean r)) r where
  runMatch res bFields arr = case uncons arr of
    Nothing -> res
    Just {head: Match b' r, tail: t} ->
      zkIf (hashFields bFields  #== hashCV (bool b'))
        (r)
        (runMatch res bFields t)

instance (Sized r, CircuitValue r) => Matchable U64 (Array (Match Int r)) r where
  runMatch res uFields arr = case uncons arr of
    Nothing -> res
    Just {head: Match u' r, tail: t} ->
      zkIf (hashFields uFields #== hashCV (u64 u'))
        (r)
        (runMatch res uFields t)

instance (Sized r, CircuitValue r) => Matchable Field (Array (Match Field r)) r where
  runMatch res fFields arr = case uncons arr of
    Nothing -> res
    Just {head: Match l' r, tail: t} ->
      zkIf (hashFields fFields #== hashFields [l'])
        (r)
        (runMatch res fFields t)


mkMatch :: forall t r. t -> r -> Match t r
mkMatch t r = Match t r

infixl 0 mkMatch as ==>

-- make this generic so you don't need instances for everything

instance Sized r => Matchable ZkUnit  (Match ZkUnit r) r where
  runMatch def u (Match _ r) = r
