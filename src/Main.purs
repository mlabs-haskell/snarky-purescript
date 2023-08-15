module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Effect.Console (log)

import Data.Foldable (foldM)

import SnarkyPS.Lib.Prelude

{- Toy game example.

  Game is a guessing game with a board:
      1 2 3
   1 | | | |
     |-----|
   2 | |x| |
     |-----|
   3 | | | |

  "Private" player picks a starting position and goal position on a 3x3 grid.

  "Public" player tries to guess a series of 4 moves (can pass) to get from the start pos to the goal pos.

  Written to illustrate features, not for elegance/performance/etc.
-}

{- Overview

  The general idea is that we want to construct a `Circuit pub priv`, where `pub` and `priv` are more-or-less normal PS types
  using the `mkCircuit` function.

  Once we have a `Circuit pub priv`, we can (attempt to) generate a proof by applying public and private inputs
  using the `prove` function. You can think of the type of prove as `prove :: Circuit pub priv -> pub -> Priv -> Aff (Prove pub priv)`

  The "magic" that powers the DSL consists in the argument to the `mkCircuit` function, which has the full type:

    ```
    mkCircuit :: forall pub priv pub' priv'
             . AsFieldsOf pub pub'
             => AsFieldsOf priv priv'
             => CircuitValue pub
             => CircuitValue priv
             => (pub' -> priv' -> ZkM Unit)
             -> Circuit pub priv
    ```

  You don't need to understand that type signature to use the DSL, but it illustrates something important: `mkCircuit` constructs a
  `Circuit pub priv` from a PureScript function that accepts the *AsFieldsOf representations* of the annotated types for arguments.

  The `AsFieldsOf` representation for a type is very easy to determine:
    a. The `AsFieldsOf` representation of all `Record`s is a `ZStruct` parameterized by a row with the same labels & the
       `AsFieldsOf` representation of elements at each label.

    b. The `AsFieldsOf` representation for all `Variant`s is a `ZEnum` parameterized by a row with the same labels & the
       `AsFieldsOf` representation of elements at each label.

    c. The `AsFieldsOf` representation of any `FieldLike` type is just that type itself.

  So, informally, the `AsFieldsOf` representation of a type `t` is a ZEnum if `t` is a Variant,
  a `ZStruct` if `t` is a Record, and just `t` if `t` is a simple FieldLike. The compiler automatically
  solves the `AsFieldsOf` typeclass due to functional dependencies, so you should never have to annotate types
  within the Circuit function.
-}

type Option a = Variant (some :: a, none :: ZUnit)

type Position = {x :: U64, y :: U64}

type Board = {start :: Position, goal :: Position}

type Move = Variant (up :: ZUnit, down :: ZUnit, left :: ZUnit, right :: ZUnit)

type Moves = { move1 :: Option Move
             , move2 :: Option Move
             , move3 :: Option Move
             , move4 :: Option Move }

{- NOTES:

  a) You generally want to construct a circuit as I do below. That is:
    - Give the entire circuit a type signature that indicates the (vanilla PureScript)
      public/private input types. This is really important - the Circuit DSL has *amazingly* good
      type inference *so long as you give a top level type*

    - Write a function `pubF -> privF -> ZkM Unit` in the ZkM monad and apply `mkCircuit` to that function

    - Note that pubF and privF are the `AsFieldsOf` representations of the PureScript types
      annotated in for the circuit and can be determined in accordance with the above procedure
      (though type inference should be strong enough that you never have to annotate them).

      See the comments below for concrete examples of the `AsFieldsOf` representation for input types

  b) Don't worry too much about what the `ZkM` monad is. At the end of the day, it's just a (complicated) trick
     to emulate a constrained Monad in PureScript. You can think of it loosely as a variation of the `Identity`
     Monad that lacks a `runIdentity` function. (Under the hood we there is a special constrained equivalent to `runIdentity`,
     but as a user you have no reason to ever extract anything from the Monad)
-}
gameCircuit :: Circuit Moves Board
gameCircuit = mkCircuit $ \moves board -> do
      {- startPos :: `ZStruct (x :: U64, y :: U64)`

        This illustrates that `get` always returns the AsFieldsOf representation of the field at its label.
        (This is why `get` is Monadic but `set` and `over` are pure.)
      -}
      startPos <- get @"start" board

      {- assertM :: ZkM Assertion -> ZkM Unit

        If you're a functional programmer, `assertM` is just an alias for `Control.Monad.void`
      -}
      assertM $ checkPos "bad start" startPos

      goal  <- get @"goal" board

      assertM $ checkPos "bad goal" goal

      {- The type of move1,move2,move3,move4 is:

          `ZEnum (some :: ZEnum (up :: ZUnit, down :: ZUnit, left :: ZUnit, right :: ZUnit), none :: ZUnit)`

        Which is the `AsFieldsOf` representation of `Option Move`. Again, you shouldn't ever need to annotate this,
        and the DSL is designed in such a way that you should really never have to *think* about the AsFieldsOf representation.

        Again, FunDeps make type inference strong enough that you should never need to add type annotations in
        the body of a circuit function - though it may be helpful to add type annotations for debugging if you
        get a confusing error. (The more type information you provide, the more likely you are to get a
        *useful* error message.)
      -}
      move1 <- get @"move1" moves
      move2 <- get @"move2" moves
      move3 <- get @"move3" moves
      move4 <- get @"move4" moves

      {- ZkM is a monad and we can do normal monad things with it, e.g. foldM/mapM/etc -}
      endPos <- foldM runMove startPos [move1,move2,move3,move4]

      assertM $ checkOutcome goal endPos
     where
      {- checkPos :: String -> ZStruct (x :: U64, y :: U64) -> ZkM Assertion -}
      checkPos msg pos = do
        posX <- get @"x" pos
        posY <- get @"y" pos
        let p x = (x #> u64 0) && (x #<= u64 3) -- The #-prefixed Ord operators return Bool (the circuit value) and not Boolean (the PS value)
        pure $ assertTrue msg (p posX && p posY) -- Bool has a HeytingAlgebra instance so you can use `not/&&/||` and so on to combine Bool values

      {- checkOutcome :: ZStruct (x :: U64, y :: U64) -> ZStruct (x :: U64, y :: U64) -> ZkM Assertion -}
      checkOutcome goalCoords endCoords = do
        gX <- get @"x" goalCoords
        gY <- get @"y" goalCoords

        eX <- get @"x" endCoords
        eY <- get @"y" endCoords

        pure $ assertTrue "player loses" (gX #== eX && gY #== eY)

      {-
      runMove :: ZStruct (x :: U64, y :: U64)
              -> ZEnum (some :: ZEnum (up :: ZUnit, down :: ZUnit, left :: ZUnit, right :: ZUnit), none :: ZUnit)
              -> ZkM (ZStruct (x :: U64, y :: U64))

      **SEE "NOTES ON PATTERN MATCHING" BELOW
      -}
      runMove start mabMove = pure $ caseOn start mabMove {
                none: zUnit ==>  start,
                some: {
                   up: zUnit ==> over @"y" minusOne start,
                   down: zUnit ==> over @"y" plusOne start,
                   left: zUnit ==> over @"x" minusOne start,
                   right: zUnit ==> over @"x" plusOne start
                  }
                }
        where
          {- You can add/subtract/multiply U64s using normal arithmetic operators
             since U64 has Semiring/Ring instances
          -}
          minusOne x =  x - (u64 1)
          plusOne x = x + (u64 1)

testBoard :: Board
testBoard = {start: {x: u64 2, y: u64 2}, goal: {x: u64 2, y: u64 1}}

{- Sums and Enumerations are represented in PS as Data.Variant Variants (with a `ZEnum` AsFields representation)

  For convenience, users can construct Variants using the provided `inj` and `inj_` functions:
    `inj` is just like Data.Variant.Inj except it uses a type application instead of a Proxy Argument.

    `inj_` is just like `inj` for labels with a `ZUnit` argument (so that you don't
    have to manually pass a `zUnit` arg)
-}
testMoves :: Moves
testMoves = {move1: inj @"some" up , move2: nullMove, move3: nullMove, move4: nullMove}
  where
    nullMove :: Option Move
    nullMove = inj_ @"none"
    up :: Move
    up = inj_ @"up"

main :: Effect Unit
main =  launchAff_  do
  proof <- prove gameCircuit testMoves testBoard
  liftEffect $ debug proof

{- NOTES ON PATTERN MATCHING

  This DSL provides a *very limited* form of pattern matching. If you're coming from a language with
  sophisticated pattern matching (any functional language, Rust, etc) you likely won't have a good
  intuition for the limitations, so here's an overview of how matching works here:

  The function you use to pattern match on Circuit values is (terrifying type sig incoming):

    `caseOn :: forall @t @m @r. CircuitValue r => CircuitValue t => Matchable t m r => r -> t -> m -> r`

  `r` is the result type of the match, and you *must* provide a default result value when matching. If
  your Matcher Expression (see below) does not yield any matches, the default result value is returned.

  `t` is the type of the value you wish to match on.

  `m` is a Matcher Expression. Its type depends upon the type of `t`, in the following manner:

     - If `t` is a ZEnum (i.e. the Circuit representation of a Variant), then `m` should be a *Record* with the
       same labels as the ZEnum (or original variant), where each label of that record should be associated with a
       record field that contains a Matcher Expression for the corresponding Variant/ZEnum label.

     - If `t` is ZUnit, `m` should have the type `Match ZUnit r`. The `==>` operator constructs a match, so if you are
       matching on a ZUnit, you will usually construct the match like: `zUnit ==> result`.

     - If `t` is a FieldLike *other than ZUnit*, `m` should have the type `Array (Match tx r)`, where `tx` is a
       PureScript expression of a type suitable for matching with `t`. As of right now, this means:
       - The Matcher Expression for `U64` is `Array (Match Int r)`
       - The Matcher Expression for 'Bool' is `Array (Match Boolean r)`
       - The Matcher Expression for 'Field' is `Array (Match Field r)`

     - At the moment, you can't match on `ZStruct`s. There's no reason why that couldn't be implemented, but it seems
       better to require users to extract fields with `get` than to match on an entire ZStruct.

  `runMove` illustrates matcher expressions for Variants and ZUnit. If we wanted to match on a `Bool` field, we could do it like:
      caseOn default (myBool :: Bool) [
        true ==> (...),
        false ==> (...)
      ]

  Similarly, if you wanted to match on a `U64` field, you'd do it like:
     caseOn default (myU64 :: U64) [
       0 ==> (...),
       1 ==> (...),
       2 ==> (...)
     ]

  The U64 example reveals one fundamental limitation of pattern matching in a Circuit:
    *** All matches must be literal matches ***

  If you're used to Haskell/PureScript/Rust/etc, you're likely familiar with *pattern variables*
  (even if you don't know they're called that). E.g., in Haskell, you can do things like
    ```
    case (x :: Maybe Int) of
      Just y -> y + 1
      Nothing -> 0
    ```
  Where `y` in that example is a *pattern variable* on the left side of the arrow that gets "transformed into" an
  Expression Variable on the right hand side. ***You can't use pattern variables in the DSL***. As far as I can tell,
  this is an inherent limitation of the proof system upon which SnarkyJS is built (though I am continuing to
  conduct research in the hopes that I will discover a way around the limitation).

  Note a consequence of this limitation: If you have a

    `a :: ZEnum (some :: x, none :: ZUnit)`

  You can *only extract a value of type `x` if you can provide a literal match for ALL values of type `x`*. `caseOn` is
  ultimately syntactic sugar over `zkIf`, which is our translation of the SnarkyJS Provable.if. In most
  circumstances, it is only *useful* to match on (small) finitely-enumerable types such ZEnums that
  correspond to user-defined Variants, or primitive FieldLikes that can be finitely enumerated (e.g. Bool).

  The above considerations entail that there is no exhaustiveness check - You will get the default value if you miss a
  match branch in your Matcher Expression.
-}
