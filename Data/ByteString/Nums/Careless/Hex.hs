

{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
  #-}


module Data.ByteString.Nums.Careless.Hex where


import Prelude hiding (head, tail, drop)
import Data.Word
import Data.Int
import Data.ByteString hiding (head, pack)
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from hexadecimal strings. Characters that are not
    hexadecimal digits are skipped over. One pleasant consequence of this is
    that a leading @0x@ is simply ignored.
 -}
class (Num n) => Hexable b n where
  hex                       ::  b -> n

instance Hexable ByteString Word8 where
  hex                        =  strict_hex
instance Hexable ByteString Word16 where
  hex                        =  strict_hex
instance Hexable ByteString Word32 where
  hex                        =  strict_hex
instance Hexable ByteString Word64 where
  hex                        =  strict_hex
instance Hexable ByteString Word where
  hex                        =  strict_hex
instance Hexable ByteString Int8 where
  hex                        =  strict_hex
instance Hexable ByteString Int16 where
  hex                        =  strict_hex
instance Hexable ByteString Int32 where
  hex                        =  strict_hex
instance Hexable ByteString Int64 where
  hex                        =  strict_hex
instance Hexable ByteString Int where
  hex                        =  strict_hex
instance Hexable ByteString Float where
  hex                        =  strict_hex
instance Hexable ByteString Double where
  hex                        =  strict_hex
instance Hexable ByteString Rational where
  hex                        =  strict_hex
instance Hexable ByteString Integer where
  hex                        =  strict_hex

instance Hexable Lazy.ByteString Word8 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Word16 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Word32 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Word64 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Word where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Int8 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Int16 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Int32 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Int64 where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Int where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Float where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Double where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Rational where
  hex                        =  lazy_hex
instance Hexable Lazy.ByteString Integer where
  hex                        =  lazy_hex




hexalize                    ::  (Num n) => n -> Word8 -> n
hexalize acc byte
  | between 'a' 'f'          =  place_up (byte + 0x0a - c2w 'a')
  | between 'A' 'F'          =  place_up (byte + 0x0a - c2w 'A')
  | between '0' '9'          =  place_up (byte - c2w '0')
  | otherwise                =  acc
 where
  between a z                =  byte >= c2w a && byte <= c2w z
  place_up b                 =  (0x10 * acc) + fromIntegral b

strict_hex                  ::  (Num n) => ByteString -> n
strict_hex bytes             =  foldl' hexalize 0 bytes

lazy_hex                    ::  (Num n) => Lazy.ByteString -> n
lazy_hex bytes               =  Lazy.foldlChunks (foldl' hexalize) 0 bytes


