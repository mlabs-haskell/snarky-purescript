module SnarkyPS.Lib.CircuitValue.Match (
  -- Exhaustive pattern matching
    class MatchE
  , caseOn_
  , caseOn
  , switch

  -- Partial pattern matching
  , Match
  , class Matchable
  , match_
  , match
  , class MkMatch
  , mkMatch
  , (==>)
  ) where

import Prelude (($),(<<<),const)

import Data.BigInt (fromInt) as BI
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(Proxy))
import Type.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Row (class Cons, class Lacks) as PR
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (drop, uncons)
import Type.Data.Peano.Nat (class IsNat, reflectNat)
import Record (get, delete) as Rec
import Data.Maybe (Maybe(Just,Nothing))

import SnarkyPS.Lib.Field (Bool, field, toBoolean)
import SnarkyPS.Lib.FieldClasses (class ZkEq, ZUnit, (#==))
import SnarkyPS.Lib.CircuitValue.Class (class CircuitValue, class CircuitValueEL, class Index, class NonEmpty, AsFields, ZEnum, asFields, forgetEnum, unFields, unsafeHead)
import SnarkyPS.Lib.Provable (witness)


{-
| Class for pattern matching on ZEnums. We use the method to
| implement *exhaustive* pattern matching, which can only be
| implemented for ZEnums
TODO: Verify that the use of `witness` here isn't going to cause any problems!
-}
class MatchE :: RowList Type -> Row Type -> RowList Type -> Row Type -> Type -> Constraint
class MatchE pList pRow zList zRow result | pList -> pRow, zList -> zRow, pList -> zList, zList -> pList where
  -- | Don't use this directly. Use `caseOn` or `switch` instead.
  caseOn_ :: Proxy pList -> Proxy zList -> Record pRow -> ZEnum zRow  -> result

instance (
    IsSymbol l
  , RowToList r (Cons l (a -> res) Nil)
  , RowToList r' (Cons l a Nil)
  , PR.Cons l (a -> res) () r
  , PR.Cons l a () r'
  , CircuitValue res
  , CircuitValue a
  ) => MatchE (Cons l (a -> res) Nil) r (Cons l a Nil) r' res where
  caseOn_ _ _ pRec zEnum =
    witness $ \ _ ->
       let rawFields = forgetEnum zEnum

           aFields :: AsFields a
           aFields = unsafeCoerce $ drop 1 rawFields

           f :: (a -> res)
           f = Rec.get label pRec

           label :: Proxy l
           label = Proxy
       in f (unFields aFields)
else instance (
    IsSymbol l
  , RowToList pRowFull (Cons l (a -> res)  pListRest)
  , RowToList zRowFull (Cons l a zListRest)
  , PR.Lacks l pRowRest
  , PR.Cons l (a -> res) pRowRest pRowFull
  , PR.Cons l a zRowRest zRowFull
  , MatchE pListRest pRowRest zListRest zRowRest res
  , Index l (Cons l a zListRest) zRowFull ix
  , IsNat ix
  , CircuitValue res
  , CircuitValue a
  ) => MatchE (Cons l (a -> res) pListRest) pRowFull (Cons l a zListRest) zRowFull res where
  caseOn_ _ _ pRec zEnum = witness $ \ _ ->
    let l :: Proxy l
        l = Proxy

        f :: (a -> res)
        f = Rec.get l pRec

        fields = forgetEnum zEnum

        i = unsafeHead fields

        ix = field <<< BI.fromInt $ reflectNat (Proxy :: Proxy ix)

        -- Maybe we should `take sizeT fields` here? Not sure if it matters?
        valFields :: AsFields a
        valFields = unsafeCoerce $ drop 1 fields

        g = caseOn_  pListRest zListRest (Rec.delete l pRec)

        pListRest = Proxy :: Proxy pListRest

        zListRest = Proxy :: Proxy zListRest
    in if (toBoolean $ i #== ix)
       then (f $ unFields valFields)
       else (g (unsafeCoerce zEnum :: ZEnum zRowRest))

{-
| Given a ZEnum and a Record of functions,
| where the ZEnum and the Record have the same labels,
| and where each field of the record contains a function
| `a -> res` such that `a` is the element at the
| corresponding label of the ZEnum, perform exhaustive
| pattern matching on the ZEnum.
-}
caseOn :: forall (z :: Row Type) (r :: Row Type) list zList res
       . RowToList z zList
       => RowToList r list
       => MatchE list r zList z res
       => ZEnum z -> Record r -> res
caseOn e r = caseOn_ (Proxy :: Proxy list) (Proxy :: Proxy zList) r e

{-
| Like `caseOn` with the order of arguments flipped.
|
| Useful for deconstructing nested ZEnums - lets you write
| `label: switch {...}` instead of `label: \val -> caseOn val {...}`
-}
switch :: forall (z :: Row Type) (r :: Row Type) list zList res
       . RowToList z zList
       => RowToList r list
       => MatchE list r zList z res
       => Record r -> ZEnum z -> res
switch r e = caseOn e r

{-
| A data type to represent partial matches. User should not need
| to construct this directly.
-}

data Match t r = Match (t -> Bool) (t -> r)

{-
| A class that relates matchable types to other types
| which can be used to match on them.
|
| `t` is the type to be matched on, `m` is the second
| type used to facilitate matching, and `res` is the
| result type of a successful application of `m` to `t`
| by `match_`.
-}
class Matchable :: Type -> Type -> Type -> Constraint
class Matchable (t :: Type) (m :: Type) (res :: Type) | m -> t where
  -- | Don't use this directly. Use `match` instead.
  match_ :: res -> AsFields t  -> m -> res

instance (
      MatchE pList pRow zList zRow result
    , NonEmpty zList
    , CircuitValueEL zList zRow
    , RowToList pRow pList
    , RowToList zRow zList
    , CircuitValue result
    ) => Matchable (ZEnum zRow) (Record pRow) result where
  match_ _ e r = caseOn (unFields e) r
else instance (CircuitValue t, CircuitValue r) => Matchable t (Match t r) r where
  match_ res asT (Match p f) = witness $ \_ ->
    if (toBoolean <<< p <<< unFields $ asT )
    then f (unFields asT)
    else res
else instance (
  CircuitValue t,
  CircuitValue r,
  Matchable t (Match t r) r
  ) => Matchable t (Array (Match t r))  r where
    match_ res asT arr = witness $ \ _ ->
      case uncons arr of
        Nothing -> res
        Just {head: Match p f, tail: rest} ->
          if (toBoolean <<< p <<< unFields $ asT)
          then f (unFields asT)
          else match_ res asT rest
else instance (
    CircuitValue r
  ) => Matchable ZUnit r r where
    match_ _ _ r = r
else instance (
    CircuitValue t
  , CircuitValue r
  ) => Matchable t (t -> r) r where
    match_ _ asT f = f (unFields asT)

{-
| Perform non-exhaustive pattern matching, with a default value in case of
| an incomplete match.
|
| `res` is the default value that will be returned if matching fails.
|
| `t` is the type you are pattern matching on.
|
|  `m` is the match expression, which will usually be a `Match` value
|  constructed with `mkMatch` or `==>`
|
|  If you want *exhaustive* pattern matching, which does not require a
|  default value and can only be performed on ZEnums, you should use
|  `caseOn` or `switch` instead of this.
-}
match :: forall (t :: Type) (m :: Type) (res :: Type)
      . Matchable t m res
      => CircuitValue t
      => res -> t -> m -> res
match defaultValue toMatch matcher = match_ defaultValue (asFields toMatch) matcher

{-
| Helper class for creating a `Match` to be consumed
| by `Matchable` instances.
-}
class MkMatch :: Type -> Type -> Type -> Type -> Constraint
class MkMatch t r t' r' | t -> t', r -> r' where
  {-
  | Constructs a `Match` to be consumed by `Matchable` instances.
  |
  | The first argument is an ingredient from which we can construct a `t -> Bool`
  | function for some `Match t r`, while the second argument is an ingredient
  | for constructing a `t -> r` function.
  |
  | At the moment `Match t r` can be constructed from:
  |   - `p :: t -> Bool` and `f :: t -> r`, which provides maximum control to the user
  |   - `p :: t -> Bool` and `res :: r`, which allows ignores the value and just returns a result if the predicate evaluates to true
  |   - `p :: t` and ``res :: r`, provided that `t` has a ZkEq instance (for literal patterns)
  |
  | You should probably use the operator version `==>`, which leads to a pleasant syntax
  -}
  mkMatch :: t -> r -> Match t' r'

instance MkMatch (t -> Bool) (t -> r) t r where
  mkMatch = Match
else instance MkMatch (t -> Bool) r t r where
  mkMatch p r = Match p (const r)
else instance ZkEq t =>  MkMatch t r t r where
  mkMatch t r = Match (\x -> x #== t) (const r)

{-| Operator alias for `mkMatch` -}
infixl 0 mkMatch as ==>
