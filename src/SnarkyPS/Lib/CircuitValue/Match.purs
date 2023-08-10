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

foreign import unsafeHead :: forall (t :: Type). Array t -> t

foreign import unsafeTail :: forall (t :: Type). Array t -> Array t

type  MayB a = (forall b. (a -> b) -> b -> b)

just :: forall a. a -> MayB a
just a =  \f _ -> f a

nothing :: forall @a. MayB a
nothing = \_ b -> b

mayB :: forall a b. MayB a -> (a -> b) -> b -> b
mayB m f b = m f b

class MatchRL :: RowList Type -> Row Type -> RowList Type -> Row Type -> Type -> Constraint
class MatchRL pList pRow zList zRow result | pList -> pRow, zList -> zRow  where
  caseOn_ :: Proxy pList -> Proxy zList -> Record pRow -> Enum zRow -> result

instance (
    IsSymbol l
  , RowToList r (Cons l (Hash -> b) Nil)
  , RowToList r' (Cons l a Nil)
  , PR.Cons l (Hash -> b) () r
  , PR.Cons l a () r'
  ) => MatchRL (Cons l (Hash -> b) Nil) r (Cons l a Nil) r' b where
  caseOn_ pList zList pRec zEnum =
    let label :: Proxy l
        label = Proxy

        f :: (Hash -> b)
        f = Rec.get label pRec

        aFields = drop 1 $ forgetEnum zEnum

        aIndex = unsafeHead aFields

        aHash = hashFields aFields
    in zkIfI_ (aIndex #== field 1)  -- We know the index here has to be 1 b/c we're out of Variant to traverse
         (f aHash)
         (f emptyHash)
else instance (
    IsSymbol l
  , RowToList pRowFull (Cons l (Hash -> b)  pListRest)
  , RowToList zRowFull (Cons l a zListRest)
  , CircuitValue b
  , PR.Lacks l pRowRest
  , PR.Cons l (Hash -> b) pRowRest pRowFull
  , PR.Cons l a zRowRest zRowFull
  , MatchRL pListRest pRowRest zListRest zRowRest b
  , Index l (Cons l a zListRest) zRowFull ix
  , IsNat ix
  ) => MatchRL (Cons l (Hash -> b) pListRest) pRowFull (Cons l a zListRest) zRowFull b where
  caseOn_ pList zList pRec zEnum = zkIfI_ (i #== ix)
     (f <<< hashFields $ valFields)
     (g (unsafeCoerce zEnum :: Enum zRowRest))
   where
     label :: Proxy l
     label = Proxy

     f :: (Hash -> b)
     f = Rec.get label pRec

     fields = forgetEnum zEnum
 
     i = unsafeHead fields
     ix = field <<< BI.fromInt $ reflectNat (Proxy :: Proxy ix)

     valFields = drop 1 fields

     g = caseOn_ pListRest zListRest (Rec.delete l pRec)

     pListRest = Proxy :: Proxy pListRest
     zListRest = Proxy :: Proxy zListRest

     noField = field (-1)
     l :: Proxy l
     l = Proxy



caseOn :: forall (pR :: Row Type) (zR :: Row Type) (pL :: RowList Type) (zL :: RowList Type) (r :: Type)
       . RowToList pR pL
       => RowToList zR zL
       => MatchRL pL pR zL zR r
       => Enum zR
       -> Record pR
       -> r
caseOn e r = caseOn_ (Proxy :: Proxy pL) (Proxy :: Proxy zL) r e


data Match t t' r = Match t (t' -> r)

class Matchable :: Type -> Type -> Type -> Constraint
class Matchable (t :: Type) (m :: Type) (res :: Type) where
  runMatch :: res -> t -> m -> res

instance Matchable Bool (Array (Match Boolean Bool r)) r where
  runMatch res b arr = case uncons arr of
    Nothing -> res
    Just {head: Match b' f, tail: t} ->
      zkIfI_ (b #== bool b')
        (f b)
        (runMatch res b t)

instance Matchable U64 (Array (Match BigInt U64 r)) r where
  runMatch res u arr = case uncons arr of
    Nothing -> res
    Just {head: Match u' f, tail: t} ->
      zkIfI_ (u #== u')
        (f u)
        (runMatch res u t)

instance Matchable Field (Array (Match Field Field r)) r where
  runMatch res l arr = case uncons arr of
    Nothing -> res
    Just {head: Match l' f, tail: t} ->
      zkIfI_ (l #== l')
        (f l)
        (runMatch res l t)
