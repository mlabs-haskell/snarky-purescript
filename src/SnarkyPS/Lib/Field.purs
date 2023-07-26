module SnarkyPS.Lib.Field  where

import Data.BigInt
import Data.BigInt as BI
import Type.Data.Symbol
import Prim.Symbol
import Type.Proxy
import Prelude
import Data.Maybe
import Effect
import Data.Argonaut.Encode.Class
import Data.Argonaut.Decode.Class


foreign import data Field :: Type

foreign import data Bool :: Type

foreign import data U32 :: Type

foreign import data U64 :: Type

foreign import data I64 :: Type

foreign import data HashInput :: Type

foreign import newField :: forall t. t -> Field

foreign import error :: forall t. String -> t

foreign import showField :: Field -> String

type DivModResult n = {quotient :: n, rest :: n}

{-
  Field functions
-}

foreign import data ConstantField :: Type

-- DO NOT EXPORT!!!! For instances only
foreign import coerceToField :: forall t. t -> Field

foreign import assertEqField :: String -> Field -> Field -> Bool

foreign import eqField :: Field -> Field -> Bool

foreign import addField :: Field -> Field -> Field

foreign import mulField :: Field -> Field -> Field

foreign import subField :: Field -> Field -> Field

foreign import toBigIntField :: Field -> BigInt

foreign import isConstantField :: Field -> Boolean

foreign import toConstantField :: Field -> ConstantField

foreign import negField :: Field -> Field

foreign import invField :: Field -> Field

foreign import isEvenField :: Field -> Bool

foreign import isOddField :: Field -> Bool

foreign import sqrtField :: Field -> Field

foreign import squareField :: Field -> Field

foreign import isZeroField :: Field -> Bool

foreign import toInputField :: Field -> HashInput

foreign import toJSONField :: Field -> String

foreign import fromJSONField :: String -> Field -- Partial / bad, need to fix things in the JS

foreign import divModField :: Field -> Field -> DivModResult Field

foreign import divField :: Field -> Field -> Field

foreign import modField :: Field -> Field -> Field

foreign import ltField :: Field -> Field -> Bool

foreign import gtField :: Field -> Field -> Bool

foreign import lteField :: Field -> Field -> Bool

foreign import gteField :: Field -> Field -> Bool

foreign import assertLtField :: String -> Field -> Field -> Bool

foreign import assertLteField :: String -> Field -> Field -> Bool

foreign import assertGtField :: String -> Field -> Field -> Bool

foreign import assertGteField :: String -> Field -> Field -> Bool

{-
  UInt64 functions
-}

-- Unsafe, for type class instances only
foreign import unsafeU64 :: forall t. t -> U64

foreign import fromFieldU64 :: Field -> U64

foreign import toBigIntU64 :: U64 -> BigInt

foreign import fromBigIntU64  :: BigInt -> U64

foreign import toFieldU64 :: U64 -> Field

foreign import toInputU64 :: U64 -> HashInput

foreign import toJSONU64 :: U64 -> String

foreign import divModU64 :: U64 -> U64 -> DivModResult U64

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

foreign import assertLtU64 :: String -> U64 -> U64 -> Bool

foreign import assertLteU64 :: String -> U64 -> U64 -> Bool

foreign import assertGtU64 :: String -> U64 -> U64 -> Bool

foreign import assertGteU64 :: String -> U64 -> U64 -> Bool

foreign import checkU64 :: U64 -> Effect Unit


{-
  Bool functions
-}

foreign import fromBoolean :: Boolean -> Bool

foreign import isConstantBool :: Bool -> Boolean

foreign import toFieldBool :: Bool -> Field

foreign import notBool :: Bool -> Bool

foreign import andBool :: Bool -> Bool -> Bool

foreign import orBool :: Bool -> Bool -> Bool

foreign import assertEqBool :: String -> Bool -> Bool -> Bool

foreign import assertTrue :: String -> Bool -> Bool

foreign import assertFalse :: String -> Bool -> Bool

foreign import equalsBool :: Bool -> Bool -> Bool

foreign import sizeInFieldsBool :: Bool -> Number

foreign import toFieldsBool :: Bool -> Array Field

foreign import toStringBool :: Bool -> String

foreign import toBoolean :: Bool -> Boolean

foreign import checkBool :: Bool -> Effect Unit

-- TODO: EncodeJson/DecodeJson instances for fields

instance Show Field where
  show = showField

instance Show U64 where
  show = showField <<< toField

instance Show Bool where
  show = show  <<< toBoolean

class MakeField :: Type -> Constraint
class MakeField t where
  field :: t -> Field

instance MakeField BigInt where
  field = coerceToField

instance MakeField Number where
  field = coerceToField

instance MakeField Field where
  field = identity

-- This is a convenience class for coercing normal PS values to U64s (we can't put this in another class)
class CoerceU64 (t :: Type) where
  u64 :: t -> U64

instance CoerceU64 BigInt where
  u64 = unsafeU64

instance CoerceU64 Int where
  u64 = unsafeU64

instance CoerceU64 Field where
  u64 = unsafeU64

class CoerceBool (t :: Type) where
  bool :: t -> Bool

instance CoerceBool Boolean where
  bool = fromBoolean

instance CoerceBool Bool where
  bool = identity

class ZkEq :: Type -> Constraint
class ZkEq t where
  zkEq :: t -> t -> Bool
  zkAssertEq :: String -> t -> t -> Bool

infix 4 zkEq as #==

class ZkOrd :: Type -> Constraint
class ZkEq t <= ZkOrd t where
  zkLT :: t -> t -> Bool
  zkAssertLT :: String -> t -> t -> Bool
  zkLTE :: t -> t -> Bool
  zkAssertLTE :: String -> t -> t -> Bool
  zkGT  :: t -> t -> Bool
  zkAssertGT :: String -> t -> t -> Bool
  zkGTE :: t -> t -> Bool
  zkAssertGTE :: String -> t -> t -> Bool

infixl 4 zkLT as #<
infixl 4 zkLTE as #<=
infixl 4 zkGT as #>
infixl 4 zkGTE as #>=

instance ZkEq Field where
  zkEq = eqField
  zkAssertEq = assertEqField

instance ZkEq U64  where
  zkEq u1 u2 = eqField (toFieldU64 u1) (toFieldU64 u2)
  zkAssertEq msg u1 u2 = assertEqField msg (toField u1) (toField u2)

instance ZkEq Bool where
  zkEq = equalsBool
  zkAssertEq = assertEqBool

instance ZkOrd Field where
  zkLT = ltField
  zkAssertLT = assertLtField
  zkLTE = lteField
  zkAssertLTE = assertLteField
  zkGT = gtField
  zkAssertGT = assertGtField
  zkGTE = gteField
  zkAssertGTE = assertGteField

instance ZkOrd U64 where
  zkLT = ltU64
  zkAssertLT = assertLtU64
  zkLTE = lteU64
  zkAssertLTE = assertLteU64
  zkGT = gtU64
  zkAssertGT = assertGtU64
  zkGTE = gteU64
  zkAssertGTE = assertGteU64

instance ZkOrd Bool where
  zkLT b1 b2 = toField b1 #< toField b2
  zkAssertLT msg b1 b2 = assertLtField msg (toField b1) (toField b2)
  zkLTE b1 b2 = toField b1 #<= toField b2
  zkAssertLTE msg b1 b2 = assertLteField msg (toField b1) (toField b2)
  zkGT b1 b2 = toField b1 #> toField b2
  zkAssertGT msg b1 b2 = assertGtField msg (toField b1) (toField b2)
  zkGTE b1 b2 = toField b1 #>= toField b2
  zkAssertGTE msg b1 b2 = assertGteField msg (toField b1) (toField b2)

-- TODO: Ring/Semiring instances for Field

instance Semiring U64 where
  add = addU64
  zero = fromBigIntU64 (fromInt 0)
  mul = mulU64
  one = fromBigIntU64 (fromInt 1)

instance Ring U64 where
  sub = subU64

class FieldLike :: Type -> Constraint
class (ZkEq t, ZkOrd t) <= FieldLike t  where
  toField :: t -> Field
  fromField :: Field -> t -- partial
  checkField :: t -> Effect Unit

-- BigInts go to U64s, Ints go to U32s

instance FieldLike Field where
  toField = identity
  fromField = identity
  checkField _ = pure unit

instance FieldLike U64  where
  toField = toFieldU64
  fromField = fromFieldU64
  checkField = checkU64

instance FieldLike Bool where
  toField = toFieldBool
  fromField b = case toNumber <<< toBigIntField  $ b of
    0.0 -> bool false
    1.0 -> bool true
    other -> error ("Error: " <> show (toField b) <> " cannot be converted to a Bool!")
  checkField = checkBool

safeStringToField :: forall (s :: Symbol). DigitSym s => Proxy s -> Field
safeStringToField proxy = case BI.fromString $ reflectSymbol proxy of
    Nothing -> error "impossible"
    Just x -> field x

class IsSymbol s <= DigitSym (s :: Symbol)

instance DigitSym "0"
else instance DigitSym "1"
else instance DigitSym "2"
else instance DigitSym "3"
else instance DigitSym "4"
else instance DigitSym "5"
else instance DigitSym "6"
else instance DigitSym "7"
else instance DigitSym "8"
else instance DigitSym "9"
else instance (DigitSym l, DigitSym r, IsSymbol o, Append l r o) => DigitSym o

