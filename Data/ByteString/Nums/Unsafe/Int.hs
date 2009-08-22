

{-# LANGUAGE MultiParamTypeClasses
  #-}


module Data.ByteString.Nums.Unsafe.Int where


import Prelude hiding (head, tail)
import Data.Word
import Data.Int
import Data.Ratio
import Data.ByteString hiding (head)
import Data.ByteString.Char8 (head)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from integer strings. 
 -}
class (Num n) => Intable b n where
  int                       ::  b -> n

instance Intable ByteString Word8 where
  int                        =  strict_int
instance Intable ByteString Word16 where
  int                        =  strict_int
instance Intable ByteString Word32 where
  int                        =  strict_int
instance Intable ByteString Word64 where
  int                        =  strict_int
instance Intable ByteString Word where
  int                        =  strict_int
instance Intable ByteString Int8 where
  int                        =  strict_int
instance Intable ByteString Int16 where
  int                        =  strict_int
instance Intable ByteString Int32 where
  int                        =  strict_int
instance Intable ByteString Int64 where
  int                        =  strict_int
instance Intable ByteString Int where
  int                        =  strict_int
instance Intable ByteString Float where
  int                        =  strict_int
instance Intable ByteString Double where
  int                        =  strict_int
instance Intable ByteString Rational where
  int                        =  strict_int
instance Intable ByteString Integer where
  int                        =  strict_int

instance Intable Lazy.ByteString Word8 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Word16 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Word32 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Word64 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Word where
  int                        =  lazy_int
instance Intable Lazy.ByteString Int8 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Int16 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Int32 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Int64 where
  int                        =  lazy_int
instance Intable Lazy.ByteString Int where
  int                        =  lazy_int
instance Intable Lazy.ByteString Float where
  int                        =  lazy_int
instance Intable Lazy.ByteString Double where
  int                        =  lazy_int
instance Intable Lazy.ByteString Rational where
  int                        =  lazy_int
instance Intable Lazy.ByteString Integer where
  int                        =  lazy_int




digitize                    ::  (Num n) => (n -> n -> n) -> n -> Word8 -> n
{-
 {-# SPECIALIZE INLINE digitize :: Word8 -> Word8 -> Word8                  #-}
 {-# SPECIALIZE INLINE digitize :: Word16 -> Word8 -> Word16                #-}
 {-# SPECIALIZE INLINE digitize :: Word32 -> Word8 -> Word32                #-}
 {-# SPECIALIZE INLINE digitize :: Word64 -> Word8 -> Word64                #-}
 {-# SPECIALIZE INLINE digitize :: Word -> Word8 -> Word                    #-}
 {-# SPECIALIZE INLINE digitize :: Int8 -> Word8 -> Int8                    #-}
 {-# SPECIALIZE INLINE digitize :: Int16 -> Word8 -> Int16                  #-}
 {-# SPECIALIZE INLINE digitize :: Int32 -> Word8 -> Int32                  #-}
 {-# SPECIALIZE INLINE digitize :: Int64 -> Word8 -> Int64                  #-}
 {-# SPECIALIZE INLINE digitize :: Int -> Word8 -> Int                      #-}
 {-# SPECIALIZE INLINE digitize :: Float -> Word8 -> Float                  #-}
 {-# SPECIALIZE INLINE digitize :: Double -> Word8 -> Double                #-}
 {-# SPECIALIZE INLINE digitize :: Rational -> Word8 -> Rational            #-}
 {-# SPECIALIZE INLINE digitize :: Integer -> Word8 -> Integer              #-}
 -}
digitize op acc byte         =  acc * 10 `op` fromIntegral (byte - 0x30)

strict_int bytes             =  foldl' (digitize op) 0 piece
 where
  (op, piece)
    | head bytes == '-'      =  ((-), tail bytes)
    | head bytes == '+'      =  ((+), tail bytes)
    | otherwise              =  ((+), bytes)

lazy_int bytes               =  Lazy.foldlChunks (foldl' (digitize op)) 0 piece
 where
  (op, piece)
    | Lazy.head bytes == '-' =  ((-), Lazy.tail bytes)
    | Lazy.head bytes == '+' =  ((+), Lazy.tail bytes)
    | otherwise              =  ((+), bytes)



