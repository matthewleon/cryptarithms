module Test.Data.Cryptarithm where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen.Common (genNonEmpty)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Cryptarithm (Cryptarithm(..), parse, toString)
import Data.Cryptarithm.CryptString (CryptString, cryptString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Gen (genAlphaUppercaseString)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

cryptarithmSpec :: Spec (QCRunnerEffects ()) Unit
cryptarithmSpec =
  describe "Cryptarithms" do
    it "correctly represents and parses itself" $
      quickCheck \(ArbitraryCryptarithm c) -> parse (toString c) === Just c

genCryptString :: forall m. MonadRec m => MonadGen m => m CryptString
genCryptString = unsafePartial (fromJust <<< cryptString)
                 <$> genAlphaUppercaseString

genCryptarithm :: forall m. MonadRec m => MonadGen m => m Cryptarithm
genCryptarithm = Cryptarithm <$> genNonEmpty genCryptString <*> genCryptString

newtype ArbitraryCryptarithm = ArbitraryCryptarithm Cryptarithm
instance arbitraryArbitraryCryptarithm :: Arbitrary ArbitraryCryptarithm
  where arbitrary = ArbitraryCryptarithm <$> genCryptarithm
