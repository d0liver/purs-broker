module Test.Main where

import Prelude

import Broker (broker, emit, events, flush)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (after_, it)
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

type TestDat = {
  dat :: { name :: String }
}

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] $ after_ (liftEffect flush) do
    it "Should allocate the broker" $ liftEffect do
      broker "myKey" *> pure unit

    it "Should fail to double allocate a broker" $ liftEffect do
      _ <- broker "foo"
      expectError (broker "foo")

    it "Should retain events" $ liftEffect do
      b <- broker "myKey"
      emit b "this-is-charles" { name: "Charles" }
      emit b "this-is-randall" { name: "Randall" }
      evts <- events b
      evts `shouldEqual` [
          { name: "this-is-randall", dat: { name: "Randall" } }
        , { name: "this-is-charles", dat: { name: "Charles"} }
      ]
