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

  -- Enum Helper
  , class ZkEnum
  , zkEnum

  -- Struct Helper
  , class ZkStruct
  , zkStruct

  -- Maybe
  , MaybeR
  , ZkMaybe
  , just
  , nothing

  -- Either
  , EitherR
  , ZkEither
  , left
  , right

  -- Tuple
  , TupleR
  , ZkTuple
  , tuple
  , (/\)
  , fst
  , snd
  , first
  , second

  -- Misc
  , assertEq
  , class IsStruct

  ) where

import Unsafe.Coerce -- gonna be doing a toooonnn of this

import Prelude (($))
import Data.HeytingAlgebra
import Prim.Row as PR

import Data.Variant as V

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



assertEq :: forall a. ZkEq a => a -> a -> Assertion
assertEq = zkAssertEq ""



{- "Simplified" classes for converting Variants->Enums & Records->Structs

   The second row is identical to the first row except all occurrences of `Record` are replaced
   with `Struct` and all the occurrences of `Variant` are replaced with `Enum`.

   Note that this transformation is recursive, i.e., if the row `r` in a `Record `r` contains `Record` or `Variant`s,
   the rows that parameterize those Records or Variants also get transformed (and so on).

   We only provide a transformation in the PureScript->Zk direction because there's essentially no reason
   to ever go in the other direction (and the existence of a function to do so would be misleading)

   Not entirely clear to me which direction the fundep should go...
-}
class ZkEnum :: Row Type -> Row Type -> Constraint
class ZkData V.Variant Enum r1 r2 <= ZkEnum r1 r2 | r1 -> r2 where
  zkEnum :: V.Variant r1 -> Enum r2
instance ZkData V.Variant Enum r1 r2 => ZkEnum r1 r2 where
  zkEnum = zkMorph

class ZkStruct :: Row Type -> Row Type -> Constraint
class ZkData Record Struct r1 r2 <= ZkStruct r1 r2 | r1 -> r2 where
  zkStruct :: Record r1 -> Struct r2
instance ZkData Record Struct r1 r2 => ZkStruct r1 r2 where
  zkStruct = zkMorph

class IsStruct :: Row Type -> Constraint
class ZkStruct r1 r1 <=  IsStruct r1
instance ZkStruct r1 r1 => IsStruct r1

type MaybeR a = (just :: a, nothing :: ZkUnit)

type ZkMaybe a = Enum (MaybeR a)

just :: forall (a :: Type) (a' :: Type)
     . ZkEnum (MaybeR a) (MaybeR a')
     => a -> ZkMaybe a'
just x = zkEnum (inj @"just" x :: V.Variant (MaybeR a))

nothing :: forall (@a :: Type) (a' :: Type)
     . ZkEnum (MaybeR a) (MaybeR a')
     => ZkMaybe a'
nothing = zkEnum (inj @"nothing" zkUnit :: V.Variant (MaybeR a))

type EitherR l r = (left :: l, right :: r)

type ZkEither l r = Enum (EitherR l r)

left :: forall (l :: Type) (r :: Type) (l' :: Type) (r' :: Type)
     . ZkData V.Variant Enum (EitherR l r) (EitherR l' r')
     => l
     -> ZkEither l' r'
left x = zkMorph (inj @"left" x :: V.Variant (EitherR l r))

right :: forall (l :: Type) (r :: Type) (l' :: Type) (r' :: Type)
     . ZkData V.Variant Enum (EitherR l r) (EitherR l' r')
     => r
     -> ZkEither l' r'
right x = zkMorph (inj @"right" x :: V.Variant (EitherR l r))

type TupleR a b = (fst :: a, snd :: b)

type ZkTuple a b = Struct (TupleR a b)

tuple :: forall (a :: Type) (b :: Type) (a' :: Type) (b' :: Type)
      . ZkData Record Struct (TupleR a b) (TupleR a' b')
      => a -> b -> ZkTuple a' b'
tuple a b = zkMorph {fst: a, snd: b}

infixr 6 tuple as /\

fst :: forall a b. Gettable a => Gettable b => ZkTuple a b -> a
fst tup = get @"fst" tup

snd :: forall a b. Gettable a => Gettable b => ZkTuple a b -> b
snd tup = get @"snd" tup

first :: forall (a :: Type) (b :: Type) (c :: Type)
      . Gettable a
      => Gettable b
      => Gettable c
      => ZkData Record Struct (TupleR c b) (TupleR c b)
      => (a -> c)
      -> ZkTuple a b
      -> ZkTuple c b
first f tup =
  let a :: a
      a = get @"fst" tup

      b :: b
      b = get @"snd" tup

      c :: c
      c = f a
  in c /\ b

second :: forall (a :: Type) (b :: Type)  (c :: Type)
      . Gettable a
      => Gettable b
      => Gettable c
      => ZkData Record Struct (TupleR a c) (TupleR a c)
      => (b -> c)
      -> ZkTuple a b
      -> ZkTuple a c
second f tup =
  let a :: a
      a = get @"fst" tup

      b :: b
      b = get @"snd" tup

      c :: c
      c = f b
  in a /\ c
