module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import SnarkyPS.Lib.Bool
import SnarkyPS.Lib.Field

main :: Effect Unit
main = do
  let b = newBool true
      bStr = "BEWL: " <> boolToString b
  log bStr
  log "🍝"
