module Main where

import Prelude hiding (Void)

import Effect (Effect)
import Effect.Class
import Effect.Aff
import Effect.Console (log)
import Type.Proxy
import Data.Variant as V
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

type Coord = ZkTuple U64 U64

-- need a type to represent the board
type Board = Struct (startPos :: ZkTuple U64 U64, goal :: ZkTuple U64 U64)


-- moves are processed in the order: Up -> Down -> Left -> Right (there are better ways to represent this, written like this to show off pattern matching)
type Move
  = Enum ( up :: ZkUnit
         , down :: ZkUnit
         , left :: ZkUnit
         , right :: ZkUnit )

type Moves = Struct ( move1 :: ZkMaybe Move
                    , move2 :: ZkMaybe Move
                    , move3 :: ZkMaybe Move
                    , move4 :: ZkMaybe Move)

gameCircuit :: Circuit Board Moves
gameCircuit = mkCircuit runGame
  where
    runGame :: Board -> Moves -> Assertion
    runGame board moves =
      let startPos :: ZkTuple U64 U64
          startPos = get @"startPos" board
          startX   = fst startPos :: U64
          startY   = snd startPos :: U64

          goal     :: ZkTuple U64 U64
          goal     = get @"goal" board
          goalX    = fst goal :: U64
          goalY    = snd goal :: U64

          move1    = get @"move1" moves :: ZkMaybe Move
          move2    = get @"move2" moves :: ZkMaybe Move
          move3    = get @"move3" moves :: ZkMaybe Move
          move4    = get @"move4" moves :: ZkMaybe Move

          check :: U64 -> Bool
          check x = (x #> (u64 0)) && (x #<= (u64 3))

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

          runMove :: ZkTuple U64 U64 -> ZkMaybe Move -> ZkTuple U64 U64
          runMove start mabMove = caseOn start mabMove {
              nothing: zkUnit ==> start
            , just: {
                up: zkUnit ==> second minusOne start
              , down: zkUnit ==> second plusOne start
              , left: zkUnit ==> first minusOne start
              , right: zkUnit ==> first plusOne start
            }
          }

          validBoard :: Assertion
          validBoard = assertMany [okStart, okGoal]

          endPos :: ZkTuple U64 U64
          endPos =
            let moved1 = runMove startPos move1
                moved2 = runMove moved1 move2
                moved3 = runMove moved2 move3
            in runMove moved3 move4

          -- TODO: "generic eq" for structs and enums
          playerWins :: Bool
          playerWins =
            let endX :: U64
                endX = fst endPos

                endY :: U64
                endY = snd endPos
            in (endX #== goalX) && (endY #== goalY)
      in assertAndThen validBoard
         $ assertTrue "player lost" playerWins








type Priv = (privB :: Bool, privU :: U64)
type Pub = (pubB :: Bool, pubU :: U64)

type TestRow = (bewl :: Bool, ent :: U64)

testEnumFunc :: Enum TestRow -> Struct TestRow -> Assertion
testEnumFunc priv pub = assertTrue "enum" go
  where
    go :: Bool
    go = caseOn (bool false) priv {
        bewl: [ true ==> bool true
              , false ==> bool false ]
      , ent: [10 ==> bool true]
      }

testEnumCirc :: Circuit (Enum TestRow) (Struct TestRow)
testEnumCirc = mkCircuit testEnumFunc

testEnumProof :: Aff (Proof (Enum TestRow) (Struct TestRow))
testEnumProof = prove testEnumCirc priv pub
  where
    priv :: V.Variant TestRow
    priv = inj @"bewl" $ bool true

    pub = {bewl: bool true, ent: u64 5}


testFunc ::  Struct Priv -> Struct Pub -> Assertion
testFunc = \b1 b2 ->
  let priB = get @"privB" b1
      pubB = get @"pubB"  b2
  in assertTrue "Test 1" (priB #== pubB)

testCirc :: Circuit (Struct Priv) (Struct Pub)
testCirc = mkCircuit testFunc

testProof :: Aff (Proof (Struct Priv) (Struct Pub))
testProof = prove testCirc {privB: bool false, privU: u64 10} {pubB: bool false, pubU: u64 5}

main :: Effect Unit
main = launchAff_  do
  let board = {startPos: {fst: u64 2, snd: u64 2}, goal: {fst: u64 2, snd: u64 2}}
      nullMove :: V.Variant (MaybeR Move)
      nullMove = inj @"nothing" zkUnit
      moves = {move1: nullMove, move2: nullMove, move3: nullMove, move4: nullMove}
  proof <- prove gameCircuit board moves
  liftEffect $ debug proof
