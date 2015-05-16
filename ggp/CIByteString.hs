module CIByteString where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Function
import Data.String

bsToUpper :: B.ByteString -> B.ByteString
bsToUpper = B.map toUpper

newtype CIByteString = CIByteString { toBS :: B.ByteString }

toCI :: B.ByteString -> CIByteString
toCI = CIByteString

instance Eq CIByteString where
  (==) = (==) `on` (bsToUpper . toBS)

instance Ord CIByteString where
  compare = compare `on` (bsToUpper . toBS)

instance Show CIByteString where
  show = show . toBS

instance IsString CIByteString where
  fromString = CIByteString . fromString
