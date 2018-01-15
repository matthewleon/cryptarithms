module Data.Cryptarithm (
  Cryptarithm(..)
, parse
, toString
) where

import Prelude

import Data.Array (uncons)
import Data.Cryptarithm.CryptString as CS
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)

data Cryptarithm = Cryptarithm (NonEmpty Array CS.CryptString) CS.CryptString
derive instance genericCryptarithm :: Generic Cryptarithm _
derive instance eqCryptarithm :: Eq Cryptarithm
instance showCryptarithm :: Show Cryptarithm where
  show = genericShow

parse :: String -> Maybe Cryptarithm
parse s =
  case split (Pattern "==") s of
    [leftSide, rightSide] -> do
       sumCryptString <- CS.cryptString rightSide
       leftCrypts <- traverse CS.cryptString (split (Pattern "+") leftSide)
       neLeftCrypts <- uncons leftCrypts <#> \{head, tail} -> head :| tail
       pure $ Cryptarithm neLeftCrypts sumCryptString
    _ -> Nothing

toString :: Cryptarithm -> String
toString (Cryptarithm (l :| ls) r) =
  CS.toString l <> joinWith "" ((\cs -> " + " <> CS.toString cs) <$> ls)
  <> " == " <> CS.toString r
