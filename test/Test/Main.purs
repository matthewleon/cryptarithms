module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Data.Cryptarithm (cryptarithmSpec)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] cryptarithmSpec
