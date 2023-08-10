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

import SnarkyPS.Lib.CircuitValue
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Circuit
import SnarkyPS.Lib.Provable
import SnarkyPS.Lib.CircuitValue.Match
import SnarkyPS.Lib.Hash

type Priv = (privB :: Bool, privU :: U64)
type Pub = (pubB :: Bool, pubU :: U64)

type TestRow = (bewl :: Bool, ent :: U64)

inL :: forall @sym a r1 r2. PR.Cons sym a r1 r2 => IsSymbol sym => a -> V.Variant r2
inL = V.inj (Proxy :: Proxy sym)

testEnumFunc :: Enum TestRow -> Struct TestRow -> Assertion
testEnumFunc priv pub = assertTrue "enum" go
  where
    go :: Bool
    go = caseOn priv {
        bewl: \(hash :: Hash) -> hash #== hashFields (toFieldsBool $ bool false)
      , ent: \(hash :: Hash) -> hash #== hashFields (toFieldsBool $ bool false)
      }

testEnumCirc :: Circuit (Enum TestRow) (Struct TestRow)
testEnumCirc = mkCircuit testEnumFunc

testEnumProof :: Aff (Proof (Enum TestRow) (Struct TestRow))
testEnumProof = prove testEnumCirc priv pub
  where
    priv :: V.Variant TestRow
    priv = inL @"bewl" $ bool true

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
  proof <- testEnumProof
  liftEffect $ debug proof
