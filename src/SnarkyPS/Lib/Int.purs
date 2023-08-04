module SnarkyPS.Lib.Int (
   U64
 , class CoerceU64
 , u64
 ) where

import Prelude (class Show, show, (<<<))
import Data.BigInt
import Data.CommutativeRing

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.FieldClasses


foreign import data U64 :: Type

-- TODO: foreign import data U32 :: Type

-- TODO: foreign import data I64 :: Type


{-
  UInt64 functions
-}

{- Probably not necessary

foreign import toBigIntU64 :: U64 -> BigInt

foreign import fromBigIntU64  :: BigInt -> U64

-}

-- Unsafe, for type class instances only, don't export (if needed in another module put it in that module's ffi)
foreign import unsafeU64 :: forall t. t ->  U64

foreign import safeU64 :: forall t. t ->  U64

foreign import fromFieldU64 :: Field -> U64

foreign import toFieldU64 :: U64 -> Field

foreign import toInputU64 :: U64 -> HashInput

foreign import toJSONU64 :: U64 -> JSON

foreign import fromJSONU64 :: JSON ->  U64

foreign import divModU64 :: U64 -> U64 -> (DivModResult U64)

foreign import divU64 :: U64 -> U64 -> U64

foreign import modU64 :: U64 -> U64 -> U64

-- has overflow checking
foreign import addU64 :: U64 -> U64 -> U64

-- has underflow checking
foreign import subU64 :: U64 -> U64 -> U64

foreign import mulU64 :: U64 -> U64 -> U64

foreign import lteU64 :: U64 -> U64 -> Bool

foreign import ltU64 :: U64 -> U64 -> Bool

foreign import gtU64 :: U64 -> U64 -> Bool

foreign import gteU64 :: U64 -> U64 -> Bool

foreign import assertLtU64 :: String -> U64 -> U64 -> Assertion

foreign import assertLteU64 :: String -> U64 -> U64 -> Assertion

foreign import assertGtU64 :: String -> U64 -> U64 -> Assertion

foreign import assertGteU64 :: String -> U64 -> U64 -> Assertion

foreign import checkU64 :: U64 -> Assertion

{- Convenience class for coercing PS values to FieldLike values -}
class CoerceU64 (t :: Type) where
  u64 :: t -> U64

instance CoerceU64 BigInt where
  u64 = unsafeU64

instance CoerceU64 Int where
  u64 = unsafeU64

instance CoerceU64 Field where
  u64 = unsafeU64

{- Instances -}

instance ZkEq U64  where
  zkEq u1 u2 = eqField (toFieldU64 u1) (toFieldU64 u2)
  zkAssertEq msg u1 u2 = assertEqField msg (toField u1) (toField u2)

instance Show U64 where
  show = unsafeShowField <<< toField

instance ZkOrd U64 where
  zkLT = ltU64
  assertLT = assertLtU64
  zkLTE = lteU64
  assertLTE = assertLteU64
  zkGT = gtU64
  assertGT = assertGtU64
  zkGTE = gteU64
  assertGTE = assertGteU64

instance FieldLike U64  where
  toField = toFieldU64
  fromField = fromFieldU64
  checkField = checkU64


{- Numeric class instances. Allows users to use +,*,-,/ instead of new operators or functions when
   performing arithmetic on FieldLikes
-}
-- TODO: Ring/Semiring instances for Field

-- These instances may violate the laws due to over/underflow, should be fine tho
instance Semiring U64 where
  add = addU64
  zero = unsafeU64 0
  mul = mulU64
  one = unsafeU64 1

instance Ring U64 where
  sub = subU64

-- no methods?
instance CommutativeRing U64

-- TODO: EuclideanRing for division. Need to implement `degree` in JS
