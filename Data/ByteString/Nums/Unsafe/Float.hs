

{-# LANGUAGE MultiParamTypeClasses
  #-}


module Data.ByteString.Nums.Unsafe.Float where


import Prelude hiding (head, tail, drop)
import Data.Word
import Data.Int
import Data.Ratio
import Data.ByteString hiding (head, pack)
import Data.ByteString.Char8 hiding (foldl')
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from hexadecimal strings.
 -}
class (Fractional f) => Floatable b f where
  float                     ::  b -> f

instance Floatable ByteString Float where
  float                      =  strict_float
instance Floatable ByteString Double where
  float                      =  strict_float
instance Floatable ByteString Rational where
  float                      =  strict_float

instance Floatable Lazy.ByteString Float where
  float                      =  lazy_float
instance Floatable Lazy.ByteString Double where
  float                      =  lazy_float
instance Floatable Lazy.ByteString Rational where
  float                      =  lazy_float




floatize                    ::  (Fractional f) => f -> Word8 -> f
{-
 {-# SPECIALIZE INLINE floatize :: Float -> Word8 -> Float                  #-}
 {-# SPECIALIZE INLINE floatize :: Double -> Word8 -> Double                #-}
 {-# SPECIALIZE INLINE floatize :: Rational -> Word8 -> Rational            #-}
 -}
floatize op acc byte         =  (acc * 10) `op` fromIntegral (byte - 0x30)

strict_float bytes           =  foldl' (floatize op) 0 piece
 where
  (op, piece)
    | head bytes == '-'      =  ((-), tail bytes)
    | head bytes == '+'      =  ((+), tail bytes)
    | otherwise              =  ((+), bytes)

lazy_float bytes             =  Lazy.foldlChunks (foldl' (floatize op)) 0 piece
 where
  (op, piece)
    | Lazy.head bytes == '-' =  ((-), Lazy.tail bytes)
    | Lazy.head bytes == '+' =  ((+), Lazy.tail bytes)
    | otherwise              =  ((+), bytes)


