module Main where

import Prelude hiding (Void)

import Effect (Effect)
import Effect.Class
import Effect.Aff
import Effect.Console (log)
import Type.Proxy
import Prim.Row as PR
import Type.Data.Symbol

import SnarkyPS.Lib.Prelude

{- Stupid toy game example.

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

type Coords = Pair U64 U64

-- need a type to represent the board
type Board = {startPos :: Coords, goal :: Coords}

type Move
  = Var ( up :: ZUnit
        , down :: ZUnit
        , left :: ZUnit
        , right :: ZUnit )

type Moves = { move1 :: Option (Zk Move)
             , move2 :: Option (Zk Move)
             , move3 :: Option (Zk Move)
             , move4 :: Option (Zk Move) }

-- Note to self, make a TermCont monad for Zk
gameCircuit :: Circuit Moves Board
gameCircuit = mkCircuit runGame
  where
    runGame :: Zk Moves -> Zk Board -> Assertion
    runGame moves' board' =
      let
          -- TODO: eliminate the need for these
          moves = coerceToZk moves'
          board = coerceToZk board'

          startPos = get @"startPos" board
          startX   = fst startPos
          startY   = snd startPos

          goal     = get @"goal" board
          goalX    = fst goal
          goalY    = snd goal

          move1    = get @"move1" moves
          move2    = get @"move2" moves
          move3    = get @"move3" moves
          move4    = get @"move4" moves

          check :: Zk U64 -> Bool
          check zkU64 = unZk zkU64 $ \x -> (x #> (u64 0)) && (x #<= (u64 3))

          okStart :: Assertion
          okStart = assertTrue "bad start" $ (check startX) && (check startY)

          okGoal :: Assertion
          okGoal = assertTrue "bad goal" $ (check goalX) && (check goalY)

          one :: U64
          one = u64 1

          plusOne :: U64 -> U64
          plusOne = \x -> x + one
          minusOne :: U64 -> U64
          minusOne = \x -> x - one

          runMove :: Zk Coords -> ZOption Move -> Zk Coords
          runMove start mabMove = caseOn start mabMove {
              none: zUnit ==> start
            , some: {
                up: zUnit ==>  (second minusOne start)
              , down: zUnit ==>  (second plusOne start)
              , left: zUnit ==>  (first minusOne start)
              , right: zUnit ==> (first plusOne start)
            }
          }

          validBoard :: Assertion
          validBoard = assertMany [okStart, okGoal]

          endPos :: Zk Coords
          endPos =
            let moved1 = runMove startPos move1
                moved2 = runMove moved1 move2
                moved3 = runMove moved2 move3
            in runMove moved3 move4

          -- TODO: "generic eq" for structs and enums
          playerWins :: Bool
          playerWins =
            let endX :: Zk U64
                endX = fst endPos

                endY :: Zk U64
                endY = snd endPos
            in (endX #== goalX) && (endY #== goalY)
      in assertAndThen validBoard
         $ assertTrue "player lost" playerWins

testBoard :: Board
testBoard = {startPos: {_1: u64 2, _2: u64 2}, goal: {_1: u64 2, _2: u64 1}}

testMoves :: Moves
testMoves = {move1: inj @"some"  up , move2: nullMove, move3: nullMove, move4: nullMove}
  where
    nullMove :: Option (Zk Move)
    nullMove = inj @"none" zUnit
    up :: Zk Move
    up = toZk $ inj @"up" zUnit


main :: Effect Unit
main =  launchAff_  do
  proof <- prove gameCircuit testMoves testBoard
  liftEffect $ debug proof
