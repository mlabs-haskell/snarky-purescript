module SnarkyPS.Lib.FieldClasses where

import Prelude (identity, (<<<))

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Context

{- Class of FieldLike types. The keystone of this whole embedding.

   This class should *not* be directly exposed to users. We need the `fromField` method
   to implement `fromFields` for the CircuitValue class, but in general there is no
   reason for a user to need such a function, which looks like it can be used to escape the
   snarky context.
-}

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

instance ZkOrd Bool where
  zkLT b1 b2 = toField b1 #< toField b2
  assertLT msg b1 b2 = assertLtField msg (toField b1) (toField b2)
  zkLTE b1 b2 = toField b1 #<= toField b2
  assertLTE msg b1 b2 = assertLteField msg (toField b1) (toField b2)
  zkGT b1 b2 = toField b1 #> toField b2
  assertGT msg b1 b2 = assertGtField msg (toField b1) (toField b2)
  zkGTE b1 b2 = toField b1 #>= toField b2
  assertGTE msg b1 b2 = assertGteField msg (toField b1) (toField b2)
