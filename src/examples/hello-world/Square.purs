module Circuit where

import Data.BigInt as BigInt
import Prelude (bind, discard, pure, ($))
import SnarkyPS.Lib.Circuit (Circuit, mkCircuit)
import SnarkyPS.Lib.Context (Context, Void)
import SnarkyPS.Lib.Field (Field, assertEqField, field)
import SnarkyPS.Lib.State (State, getAndAssertEqState, newState, setState)

checkSquare ::
  State Field
  -- Private Input
  -> Field
  -- We should probably give `Context` type another name since it doesn't represent
  -- the actual "context" of a zkApp. By the way I think in reality we will have
  -- some sort of state monad transformer for that purpose.
  -> Context Void
checkSquare numState guess = do
  -- Fetch value from state and check that it's valid.
  num <- getAndAssertEqState numState

  assertEqField "bad guess" num guess

  setState guess numState

init :: Context (State Field)
init = do
  state <- newState
  initial <- field $ BigInt.fromInt 3
  setState initial state
  pure state

circuit :: Circuit
  -- We ignore the public input here.
  -- It can't be `Void` cause it's not `CircuitValue`.
  Field Field
circuit = mkCircuit $ \pub pri  -> do
  zeroField <- field $ BigInt.fromInt 0
  assertEqField "public input should be 0" pub zeroField
  state <- init
  checkSquare state pri