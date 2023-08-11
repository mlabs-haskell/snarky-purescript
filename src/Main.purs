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

  First player submits a series of moves that move the x around, second player tries to guess the
  location (also by a series of moves).

  Written to illustrate features, not for elegance/performance/etc.
-}

type Coords = Pair U64 U64

-- need a type to represent the board
type Board = {startPos :: Coords, goal :: Coords}

testBoard :: Board
testBoard = {startPos: {_1: u64 2, _2: u64 2}, goal: {_1: u64 2, _2: u64 2}}


-- moves are processed in the order: Up -> Down -> Left -> Right (there are better ways to represent this, written like this to show off pattern matching)
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
          moves = coerceToZk moves'
          board = coerceToZk board'

          startPos = get @"startPos" board
          startX   = fst startPos :: Zk U64
          startY   = snd startPos ::Zk U64

          goal     = get @"goal" board
          goalX    = fst goal :: Zk U64
          goalY    = snd goal :: Zk U64

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
      in assertTrue "player lost" playerWins


main :: Effect Unit
main =  launchAff_  do
  let board = testBoard
      nullMove :: Option (Zk Move)
      nullMove = inj @"none" zUnit
      moves = {move1: nullMove, move2: nullMove, move3: nullMove, move4: nullMove}
  proof <- prove gameCircuit  moves board
  liftEffect $ debug proof
