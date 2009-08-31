

{-# LANGUAGE MultiParamTypeClasses
  #-}


module Data.ByteString.Nums.Careless.Hex where


import Prelude hiding (head, tail, drop)
import Data.Word
import Data.Int
import Data.Ratio
import Data.ByteString hiding (head, pack)
import Data.ByteString.Char8 hiding (foldl')
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from hexadecimal strings. Characters that are not
    hexadecimal digits are skipped over.
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
{-
{-# SPECIALIZE INLINE           hexalize :: Word8 -> Word8 -> Word8        #-}
{-# SPECIALIZE INLINE           hexalize :: Word16 -> Word8 -> Word16      #-}
{-# SPECIALIZE INLINE           hexalize :: Word32 -> Word8 -> Word32      #-}
{-# SPECIALIZE INLINE           hexalize :: Word64 -> Word8 -> Word64      #-}
{-# SPECIALIZE INLINE           hexalize :: Word -> Word8 -> Word          #-}
{-# SPECIALIZE INLINE           hexalize :: Int8 -> Word8 -> Int8          #-}
{-# SPECIALIZE INLINE           hexalize :: Int16 -> Word8 -> Int16        #-}
{-# SPECIALIZE INLINE           hexalize :: Int32 -> Word8 -> Int32        #-}
{-# SPECIALIZE INLINE           hexalize :: Int64 -> Word8 -> Int64        #-}
{-# SPECIALIZE INLINE           hexalize :: Int -> Word8 -> Int            #-}
{-# SPECIALIZE INLINE           hexalize :: Float -> Word8 -> Float        #-}
{-# SPECIALIZE INLINE           hexalize :: Double -> Word8 -> Double      #-}
{-# SPECIALIZE INLINE           hexalize :: Rational -> Word8 -> Rational  #-}
{-# SPECIALIZE INLINE           hexalize :: Integer -> Word8 -> Integer    #-}
 -}
hexalize acc byte
  | between byte 'a' 'f'     =  place_up (byte + 0x0a - c2w 'a')
  | between byte 'A' 'F'     =  place_up (byte + 0x0a - c2w 'A')
  | between byte '0' '9'     =  place_up (byte - c2w '0')
  | otherwise                =  acc
 where
  between b a z              =  b >= c2w a && byte <= c2w z
  place_up b                 =  (0x10 * acc) + fromIntegral b

strict_hex bytes             =  foldl' hexalize 0 bytes

lazy_hex bytes               =  Lazy.foldlChunks (foldl' hexalize) 0 bytes


