module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Cryptarithm (parse, toString)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let crStr = "SEND + MORE == MONEY"
  log $ toString $ unsafePartial $ fromJust $ parse crStr
