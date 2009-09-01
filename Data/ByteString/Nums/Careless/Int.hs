

{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , BangPatterns
  #-}


module Data.ByteString.Nums.Careless.Int where


import Prelude hiding (head, tail, null)
import Data.Word
import Data.Int
import Data.Ratio
import Data.ByteString hiding (head, pack)
import Data.ByteString.Internal
import Data.ByteString.Char8 hiding (foldl')
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from integer strings. Characters that are not
    decimal digits are simply skipped.
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
digitize op acc byte
  | between '0' '9'          =  (acc * 10) `op` fromIntegral (byte - c2w '0')
  | otherwise                =  acc
 where
  between a z                =  byte >= c2w a && byte <= c2w z

strict_int bytes             =  foldl' (digitize op) 0 piece
 where
  (op, piece)
    | null bytes             =  ((+), empty)
    | head bytes == '-'      =  ((-), tail bytes)
    | head bytes == '+'      =  ((+), tail bytes)
    | otherwise              =  ((+), bytes)


lazy_int bytes               =  Lazy.foldlChunks dfold 0 piece
 where
  (op, piece)
    | Lazy.null bytes        =  ((+), Lazy.empty)
    | Lazy.head bytes == '-' =  ((-), Lazy.tail bytes)
    | Lazy.head bytes == '+' =  ((+), Lazy.tail bytes)
    | otherwise              =  ((+), bytes)
  dfold                      =  {-# SCC "inner_fold" #-} foldl' (digitize op)



