module SnarkyPS.Lib.Field (
  -- Core exports
    Field
  , Fields
  , SizeInFields
  , class MakeField
  , field

  -- for instances that can't go here
  , eqField
  , assertEqField
  , ltField
  , assertLtField
  , lteField
  , assertLteField
  , gtField
  , assertGtField
  , gteField
  , assertGteField
  -- TODO: Eliminate?
  , checkField_
  -- Unsafe!
  , unsafeShowField

  -- Bool stuff
  , Bool
  , fromBoolean
  , toBoolean

  -- Needed for U64 instances
  , DivModResult

  -- Misc
  , error
  )  where

import Prelude hiding (Void)

import Data.BigInt (BigInt)
import Data.BigInt as BI
import Type.Data.Symbol
import Prim.Symbol
import Type.Proxy
import Data.Maybe
import Data.HeytingAlgebra
import SnarkyPS.Lib.Context
import Unsafe.Coerce (unsafeCoerce)

foreign import data Field :: Type


{- We have to define the primitive FieldLikes here to avoid circular dependencies
   while keeping the module structure sane.
-}
foreign import data Bool :: Type

foreign import data HashInput :: Type

foreign import newField :: forall t. t -> Field

foreign import error :: forall t. String -> t

foreign import unsafeShowField :: Field -> String

type DivModResult n = {quotient :: n, rest :: n}


{- Another opaque type to prevent users from doing anything with the results of
   sizeInFields in a snarky context. The unsafe functions that convert to/from numbers
   should not be exposed

   Inner is Int
-}

foreign import data SizeInFields :: Type


{- Opaque for the result of ToFields. Again, should not be exposed.

   Inner is (Array Field)
-}
foreign import data Fields :: Type

{-
  Field functions
-}

-- DO NOT EXPORT!!!! For instances only
foreign import coerceToField :: forall t. t -> Field

foreign import eqField :: Field -> Field -> Bool

foreign import addField :: Field -> Field -> Field

foreign import mulField :: Field -> Field -> Field

foreign import subField :: Field -> Field -> Field

foreign import toBigIntField :: Field -> BigInt

foreign import negField :: Field -> Field

foreign import invField :: Field -> Field

foreign import isEvenField :: Field -> Bool

foreign import isOddField :: Field ->  Bool

foreign import sqrtField :: Field -> Field

foreign import squareField :: Field -> Field

foreign import isZeroField :: Field ->  Bool

foreign import divModField :: Field -> Field -> DivModResult Field

foreign import divField :: Field -> Field -> Field

foreign import modField :: Field -> Field -> Field

foreign import ltField :: Field -> Field -> Bool

foreign import gtField :: Field -> Field -> Bool

foreign import lteField :: Field -> Field -> Bool

foreign import gteField :: Field -> Field -> Bool

foreign import assertEqField :: String -> Field -> Field -> Assertion

foreign import assertLtField :: String -> Field -> Field -> Assertion

foreign import assertLteField :: String -> Field -> Field -> Assertion

foreign import assertGtField :: String -> Field -> Field -> Assertion

foreign import assertGteField :: String -> Field -> Field -> Assertion

foreign import checkField_ :: Field -> Assertion

{-
  Bool Functions (for instances)
-}
foreign import notBool :: Bool -> Bool

foreign import andBool :: Bool -> Bool -> Bool

foreign import orBool :: Bool -> Bool -> Bool

foreign import fromBoolean :: Boolean -> Bool

foreign import toBoolean :: Bool -> Boolean

{- Bool HeytingAlgebra Instance (so we can use && || etc directly) -}

instance HeytingAlgebra Bool where
  ff = fromBoolean false
  tt = fromBoolean true
  implies a b = not a || b
  conj = andBool
  disj = orBool
  not = notBool

-- NOTE: These show instances are necessary for debugging, but should be removed eventually
instance Show Field where
  show = unsafeShowField

instance Show Bool where
  show = show  <<< toBoolean

{- Vanilla types that can be turned into a (constant) Field. Just a convenience.  -}
class MakeField :: Type -> Constraint
class MakeField t where
  field :: t ->  Field

instance MakeField BigInt where
  field = coerceToField

instance MakeField Int where
  field = coerceToField

instance MakeField Number where
  field = coerceToField

instance MakeField Field where
  field = identity

instance DigitSym s => MakeField (Proxy s) where
  field _ = symToField @s

symToField :: forall (@s :: Symbol). DigitSym s => Field
symToField  = case BI.fromString $ reflectSymbol (Proxy :: Proxy s) of
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
