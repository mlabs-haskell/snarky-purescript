module SnarkyPS.Lib.FieldClasses where

import Prelude (identity, (<<<), ($))
import Data.HeytingAlgebra
import Unsafe.Coerce

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Hash

import Unsafe.Coerce

{- Class of FieldLike types. The keystone of this whole embedding.

   This class should *not* be directly exposed to users. We need the `fromField` method
   to implement `fromFields` for the CircuitValue class, but in general there is no
   reason for a user to need such a function, which looks like it can be used to escape the
   snarky context.
-}

hashToField :: Hash -> Field
hashToField = unsafeCoerce

fieldToHash :: Field -> Hash
fieldToHash = unsafeCoerce

class FieldLike :: Type -> Constraint
class (ZkEq t, ZkOrd t) <= FieldLike t  where
  toField :: t -> Field
  fromField :: Field -> t -- partial
  checkField :: t -> Assertion

instance FieldLike Field where
  toField = identity
  fromField = identity
  checkField = checkField_

instance FieldLike Bool where
  toField = toFieldBool
  fromField = fromFieldBool -- unsafe?
  checkField = checkBool

instance FieldLike Hash where
  toField = hashToField
  fromField = fieldToHash
  checkField h = checkField (toField h)


fieldLikeToInput :: forall (t :: Type). FieldLike t => t -> HashInput
fieldLikeToInput = toInputField <<< toField

{- Convenience class for coercing PS values to FieldLike values -}
class CoerceBool (t :: Type) where
  bool :: t -> Bool

instance CoerceBool Boolean where
  bool = fromBoolean

instance CoerceBool Bool where
  bool = identity

{- EQ for Circuit Values (more or less) -}
class ZkEq :: Type -> Constraint
class ZkEq t where
  zkEq :: t ->  t ->  Bool
  zkAssertEq :: String -> t -> t -> Assertion

infix 4 zkEq as #==

instance ZkEq Field where
  zkEq = eqField
  zkAssertEq = assertEqField

instance ZkEq Bool where
  zkEq = equalsBool
  zkAssertEq = assertEqBool

instance ZkEq Hash where
  zkEq h1 h2 = zkEq (hashToField h1) (hashToField h2)
  zkAssertEq msg h1 h2 = zkAssertEq msg (hashToField h1) (hashToField h2)

{- Ord for Circuit Values -}
class ZkOrd :: Type -> Constraint
class ZkEq t <= ZkOrd t where
  zkLT :: t -> t ->  Bool
  assertLT :: String -> t -> t -> Assertion
  zkLTE :: t -> t ->  Bool
  assertLTE :: String -> t -> t -> Assertion
  zkGT  :: t -> t -> Bool
  assertGT :: String -> t -> t -> Assertion
  zkGTE :: t -> t -> Bool
  assertGTE :: String -> t -> t -> Assertion

infixl 4 zkLT as #<
infixl 4 zkLTE as #<=
infixl 4 zkGT as #>
infixl 4 zkGTE as #>=

instance ZkOrd Field where
  zkLT = ltField
  assertLT = assertLtField
  zkLTE = lteField
  assertLTE = assertLteField
  zkGT = gtField
  assertGT = assertGtField
  zkGTE = gteField
  assertGTE = assertGteField

instance ZkOrd Hash where
  zkLT h1 h2 = ltField (hashToField h1) (hashToField h2)
  assertLT msg h1 h2 = assertLT msg (hashToField h1) (hashToField h2)
  zkLTE h1 h2 = zkLTE (hashToField h1) (hashToField h2)
  assertLTE msg h1 h2 = assertLTE msg (hashToField h1) (hashToField h2)
  zkGT h1 h2 = zkGT (hashToField h1) (hashToField h2)
  assertGT msg h1 h2 = assertGT msg (hashToField h1) (hashToField h2)
  zkGTE  h1 h2 = zkGTE (hashToField h1) (hashToField h2)
  assertGTE msg h1 h2 = assertGTE msg (hashToField h1) (hashToField h2)

instance ZkOrd Bool where
  zkLT b1 b2 = toField b1 #< toField b2
  assertLT msg b1 b2 = assertLtField msg (toField b1) (toField b2)
  zkLTE b1 b2 = toField b1 #<= toField b2
  assertLTE msg b1 b2 = assertLteField msg (toField b1) (toField b2)
  zkGT b1 b2 = toField b1 #> toField b2
  assertGT msg b1 b2 = assertGtField msg (toField b1) (toField b2)
  zkGTE b1 b2 = toField b1 #>= toField b2
  assertGTE msg b1 b2 = assertGteField msg (toField b1) (toField b2)

{- An in-circuit unit type -}

foreign import data ZkUnit :: Type

zkUnit :: ZkUnit
zkUnit = unsafeCoerce $ field 1 -- somewhat arbitrary, doesn't matter what it is

-- internal
unitField :: ZkUnit -> Field
unitField = unsafeCoerce

instance ZkEq ZkUnit where
  zkEq u1 u2 = (unitField u1 #== one) && (unitField u2 #== one)
    where
      one = field 1
  zkAssertEq msg u1 u2 = assertTrue msg (u1 #== u2)

instance FieldLike ZkUnit where
  toField = unitField
  fromField _ = zkUnit -- idk if this is the best approach here, TODO: think about this later
  checkField u = assertTrue "invalid unit rep" (field 1 #== unitField u)

instance ZkOrd ZkUnit where
  zkLT _ _ = ff
  assertLT msg _ _ = assertTrue msg ff
  zkLTE _ _ = tt
  assertLTE msg _ _ = assertTrue msg tt
  zkGT _ _ = ff
  assertGT msg _ _ = assertTrue msg ff
  zkGTE _ _ = tt
  assertGTE msg _ _ = assertTrue msg tt
