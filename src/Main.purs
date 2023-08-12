module Main where

import Prelude hiding (Void)

import Effect (Effect)
import Effect.Class
import Effect.Aff
import Effect.Console (log)
import Type.Proxy
import Prim.Row as PR
import Type.Data.Symbol

import Data.Foldable

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

  "Public" player tries to guess a series of 4 moves (can pass) to get from the start pos to the end pos.

  Written to illustrate features, not for elegance/performance/etc.
-}

{- Overview

  The general idea is that we want to construct a `Circuit `

-}

type Option a = Variant (some :: a, none :: ZUnit)

type Position = {x :: U64, y :: U64}

type Board = {start :: Position, goal :: Position}

type Move = Variant (up :: ZUnit, down :: ZUnit, left :: ZUnit, right :: ZUnit)

type Moves = { move1 :: Option Move
             , move2 :: Option Move
             , move3 :: Option Move
             , move4 :: Option Move }

gameCircuit :: Circuit Moves Board
gameCircuit = mkCircuit $ \moves board -> do
      startPos <- get @"start" board

      assertM $ checkPos "bad start" startPos

      goal  <- get @"goal" board

      assertM $ checkPos "bad goal" goal

      move1 <- get @"move1" moves
      move2 <- get @"move2" moves
      move3 <- get @"move3" moves
      move4 <- get @"move4" moves

      endPos <- foldM runMove startPos [move1,move2,move3,move4]

      assertM $ checkOutcome goal endPos
     where
      checkPos msg pos = do
        posX <- get @"x" pos
        posY <- get @"y" pos
        let p x = (x #> u64 0) && (x #<= u64 3)
        pure $ assertTrue msg (p posX && p posY)

      checkOutcome goalCoords endCoords = do
        gX <- get @"x" goalCoords
        gY <- get @"y" goalCoords

        eX <- get @"x" endCoords
        eY <- get @"y" endCoords

        pure $ assertTrue "player loses" (gX #== eX && gY #== eY)

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
          minusOne x =  x - (u64 1)
          plusOne x = x + (u64 1)




testBoard :: Board
testBoard = {start: {x: u64 2, y: u64 2}, goal: {x: u64 2, y: u64 1}}

testMoves :: Moves
testMoves = {move1: inj @"some"  up , move2: nullMove, move3: nullMove, move4: nullMove}
  where
    nullMove :: Option Move
    nullMove = inj_ @"none"
    up :: Move
    up = inj_ @"up"


main :: Effect Unit
main =  launchAff_  do
  proof <- prove gameCircuit testMoves testBoard
  liftEffect $ debug proof
