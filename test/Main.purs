module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Prelude
import Test.Control.Applicative.Fetch as Control.Applicative.Fetch
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [tapReporter] do
  Control.Applicative.Fetch.spec
