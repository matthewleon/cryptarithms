module Data.Cryptarithm.CryptString (
  CryptString
, cryptString
, toCharArray
, toString
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

newtype CryptString = CryptString (Array Char)
derive newtype instance eqCryptString :: Eq CryptString
derive newtype instance ordCryptString :: Ord CryptString
derive newtype instance semigroupCryptString :: Semigroup CryptString
instance showCryptString :: Show CryptString where
  show cs = "cryptString " <> toString cs

cryptString :: String -> Maybe CryptString
cryptString s =
  let prepped = S.toUpper $ S.trim s
      alphaRe = unsafeRegex "^[A-Z]+$" noFlags
  in if test alphaRe prepped
       then Just $ CryptString $ S.toCharArray $ prepped
       else Nothing

toCharArray :: CryptString -> Array Char
toCharArray (CryptString ca) = ca

toString :: CryptString -> String
toString = S.fromCharArray <<< toCharArray
