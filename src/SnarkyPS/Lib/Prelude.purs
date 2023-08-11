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
  , module SnarkyPS.Lib.CircuitValue.Class

  -- Option
  , Option
  , ZOption
  , some
  , none

  -- Result
  , Result
  , ZResult
  , err
  , ok

  -- Pair
  , Pair
  , ZPair
  , fst
  , snd
  , first
  , second

  -- Misc
  , assertEq
  , Var


  ) where

import Unsafe.Coerce -- gonna be doing a toooonnn of this

import Prelude
import Data.HeytingAlgebra
import Prim.Row as PR
import Prim.RowList

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
import SnarkyPS.Lib.CircuitValue.Class

assertEq :: forall a. ZkEq a => a -> a -> Assertion
assertEq = zkAssertEq ""

type Var = V.Variant

type Option a = Var (some :: a, none :: ZUnit)

-- TODO: These ZTypes should all be newtypes (but need more classes to handle them if they are)
type ZOption a = Zk (Option (Zk a))

some :: forall (a :: Type). CircuitValue a => a -> ZOption a
some x = toZk (inj @"some" (toZk x) :: Option (Zk a))

none :: forall (a :: Type). CircuitValue a => ZOption a
none = toZk (inj @"none" zUnit :: Option (Zk a))

type Result l r = Var (err :: l, ok :: r)

type ZResult l r = Zk (Result l r)

err :: forall (l :: Type) (r :: Type)
     . CircuitValue l
     => CircuitValue r
     => l
     -> ZResult l r
err x = toZk (inj @"err" x :: Result l r)

ok :: forall (l :: Type) (r :: Type)
   . CircuitValue l
   => CircuitValue r
   => r
   -> ZResult l r
ok x = toZk (inj @"ok" x :: Result l r)

type Pair a b = {_1 :: a, _2 :: b}

type ZPair a b = Zk (Pair a b)

pair :: forall (a :: Type) (b :: Type)
      . CircuitValue a
      => CircuitValue b
      => a
      -> b
      -> ZPair a b
pair a b = toZk {_1: a, _2: b}

infixr 6 pair as /\

fst :: forall a a' b. CircuitValue a => CircuitValue b  => IsZk a a' =>  ZPair a b -> Zk a'
fst tup = get @"_1" tup

snd :: forall a b b'. CircuitValue a => CircuitValue b => IsZk b b' => ZPair a b -> Zk b'
snd tup = get @"_2" tup

first :: forall (a :: Type) (a' :: Type) (b :: Type) (b' :: Type) (c :: Type)
      . CircuitValue a
      => CircuitValue b
      => CircuitValue c
      => IsZk a a'
      => IsZk b b'
      => (a -> c)
      -> ZPair a b
      -> ZPair c b
first f tup =
  let a :: Zk a
      a = coerceFromZk $ fst tup

      b :: b
      b = fromZk <<< coerceFromZk $ snd tup

      c :: c
      c = f (fromZk a)
  in c /\ b

second :: forall (a :: Type) (a' :: Type) (b :: Type) (b' :: Type) (c :: Type)
      . CircuitValue a
      => CircuitValue b
      => CircuitValue c
      => IsZk a a'
      => IsZk b b'
      => (b -> c)
      -> ZPair a b
      -> ZPair a c
second f tup =
  let a :: a
      a = fromZk <<< coerceFromZk $ fst tup

      b :: Zk b
      b = coerceFromZk $ snd tup

      c :: c
      c = f (fromZk b)
  in a /\ c


-- this is essentially a specialized Codensity monad
unZkM :: forall a b. ZkM a -> (a -> Zk b) -> Zk b
unZkM (ZkM f) = f

newtype ZkM a = ZkM (forall b. (a -> Zk b) -> Zk b)

instance Functor ZkM where
  map f (ZkM g) = ZkM (\k -> g (k <<< f))

instance Apply ZkM where
  apply (ZkM f) (ZkM g) = ZkM (\k -> f (\l -> g (k <<< l)))

instance Applicative ZkM where
  pure x = ZkM (_ $ x)

instance Bind ZkM where
  bind (ZkM f) k = ZkM (\g -> (f (\x -> unZkM (k x) g)))

instance Monad ZkM

lift :: forall (a :: Type). CircuitValue a => Zk a -> ZkM a
lift = pure <<< fromZk

lower :: forall (a :: Type). CircuitValue a => ZkM a -> Zk a
lower zk = unZkM zk toZk

getM :: forall @label t t' row list
    . GetField label t t' list row
    => RowToList row list
    => CircuitValue
    => IsZk t t' -- just to be extra sure the compiler does its job -_-
    => ZkStruct row
    -> ZkM t'
getM = lift <<< get @label
