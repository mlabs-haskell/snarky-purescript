module Main where

import Prelude hiding (Void)

import Effect (Effect)
import Effect.Class
import Effect.Aff
import Effect.Console (log)
import Type.Proxy

import SnarkyPS.Lib.CircuitValue
import SnarkyPS.Lib.Context
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Int
import SnarkyPS.Lib.Field
import SnarkyPS.Lib.FieldClasses
import SnarkyPS.Lib.Circuit
import SnarkyPS.Lib.Provable

type Priv = (privB :: Bool, privU :: U64)
type Pub = (pubB :: Bool, pubU :: U64)

type TestEnum = (a :: Bool, b :: {b1 :: U64, b2 :: U64}, c :: Bool)

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
  proof <- testProof
  liftEffect $ debug proof
