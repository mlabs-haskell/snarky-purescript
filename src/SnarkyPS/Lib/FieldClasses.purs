module SnarkyPS.Lib.FieldClasses (
  -- FieldLike
    class FieldLike
  , toField
  , fromField
  , checkField

  -- Coercion Helpers
  , class CoerceBool
  , bool

  -- ZkEq
  , class ZkEq
  , zkEq
  , zkAssertEq
  , (#==)

  -- ZkOrd
  , class ZkOrd
  , zkLT
  , assertLT
  , zkLTE
  , assertLTE
  , zkGT
  , assertGT
  , zkGTE
  , assertGTE
  , (#<)
  , (#<=)
  , (#>)
  , (#>=)

  -- ZUnit
  , ZUnit
  , zUnit
  ) where

import Prelude (identity, (<<<), ($))
import Data.HeytingAlgebra
import Unsafe.Coerce

import SnarkyPS.Lib.Field
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Hash

import Partial
import Partial.Unsafe
import Unsafe.Coerce
import Data.Maybe

hashToField :: Hash -> Field
hashToField = unsafeCoerce

fieldToHash :: Field -> Hash
fieldToHash = unsafeCoerce

{-
| A Class for `CircuitValue`s which
| have an underlying representation
| of a single `Field`
|
| Users should never need to use this directly.
-}
class FieldLike :: Type -> Constraint
class (ZkEq t, ZkOrd t) <= FieldLike t  where
  {- Coerce a `FieldLike` to a `Field`
  |
  | Not that this is *not* how you should ordinarily
  | convert vanilla PureScript types to their `CircuitValue`
  | analogues. Use the methods in `SnarkyPS.Lib.CircuitValue.Class`
  | if you want to do that.
  -}
  toField :: t -> Field
  {-
  | Coerce a Field into a FieldLike value.
  |
  | This should not be used directly by users, as it is
  | partial and unsafe, but is needed for internal purposes
  | (where it can be used safely).
  -}
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

{- | Convenience class for coercing to `Bool` -}
class CoerceBool (t :: Type) where
  {-
  | Convert a `Bolean` into a `Bool`
  |
  | This is the ordinary way to construct `Bool` values from PureScript `Boolean`s
  -}
  bool :: t -> Bool

instance CoerceBool Boolean where
  bool = fromBoolean

instance CoerceBool Bool where
  bool = identity

{- | Eq for `CircuitValue`s -}
class ZkEq :: Type -> Constraint
class ZkEq t where
  zkEq :: t -> t ->  Bool
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



{- | Ord for Circuit Values -}
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


{- An in-circuit unit type. TODO: Move somewhere else -}

foreign import data ZUnit :: Type

zUnit :: ZUnit
zUnit = unsafeCoerce $ field 1 -- somewhat arbitrary, doesn't matter what it is

-- internal
unitField :: ZUnit -> Field
unitField = unsafeCoerce

instance ZkEq ZUnit where
  zkEq u1 u2 = (unitField u1 #== one) && (unitField u2 #== one)
    where
      one = field 1
  zkAssertEq msg u1 u2 = assertTrue msg (u1 #== u2)

instance FieldLike ZUnit where
  toField = unitField
  fromField _ = zUnit -- idk if this is the best approach here, TODO: think about this later
  checkField u = assertTrue "invalid unit rep" (field 1 #== unitField u)

instance ZkOrd ZUnit where
  zkLT _ _ = ff
  assertLT msg _ _ = assertTrue msg ff
  zkLTE _ _ = tt
  assertLTE msg _ _ = assertTrue msg tt
  zkGT _ _ = ff
  assertGT msg _ _ = assertTrue msg ff
  zkGTE _ _ = tt
  assertGTE msg _ _ = assertTrue msg tt
