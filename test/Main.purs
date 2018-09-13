module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude
import Test.Control.Applicative.Fetch as Control.Applicative.Fetch
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [tapReporter] do
  Control.Applicative.Fetch.spec
