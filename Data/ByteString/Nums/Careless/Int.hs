

{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
  #-}


module Data.ByteString.Nums.Careless.Int where


import Prelude hiding (head, tail, null)
import Data.Word
import Data.Int
import Data.ByteString hiding (head, pack)
import Data.ByteString.Internal
import Data.ByteString.Char8 hiding (foldl')
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy




{-| Types that can be read from integer strings. Parses only decimal digits.
    Signed types can be read from strings that begin with a plus or minus;
    unsigned types are read from strings consisting solely of decimal digits.
 -}
class (Num n) => Intable b n where
  int                       ::  b -> n

instance Intable ByteString Word8 where
  int                        =  strict_unsigned
instance Intable ByteString Word16 where
  int                        =  strict_unsigned
instance Intable ByteString Word32 where
  int                        =  strict_unsigned
instance Intable ByteString Word64 where
  int                        =  strict_unsigned
instance Intable ByteString Word where
  int                        =  strict_unsigned
instance Intable ByteString Int8 where
  int                        =  strict_signed
instance Intable ByteString Int16 where
  int                        =  strict_signed
instance Intable ByteString Int32 where
  int                        =  strict_signed
instance Intable ByteString Int64 where
  int                        =  strict_signed
instance Intable ByteString Int where
  int                        =  strict_signed
instance Intable ByteString Float where
  int                        =  strict_signed
instance Intable ByteString Double where
  int                        =  strict_signed
instance Intable ByteString Rational where
  int                        =  strict_signed
instance Intable ByteString Integer where
  int                        =  strict_signed

instance Intable Lazy.ByteString Word8 where
  int                        =  lazy_unsigned
instance Intable Lazy.ByteString Word16 where
  int                        =  lazy_unsigned
instance Intable Lazy.ByteString Word32 where
  int                        =  lazy_unsigned
instance Intable Lazy.ByteString Word64 where
  int                        =  lazy_unsigned
instance Intable Lazy.ByteString Word where
  int                        =  lazy_unsigned
instance Intable Lazy.ByteString Int8 where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Int16 where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Int32 where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Int64 where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Int where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Float where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Double where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Rational where
  int                        =  lazy_signed
instance Intable Lazy.ByteString Integer where
  int                        =  lazy_signed




lazy_unsigned               ::  (Num n) => Lazy.ByteString -> n
lazy_unsigned                =  Lazy.foldlChunks (foldl' positive) 0

lazy_signed                 ::  (Num n) => Lazy.ByteString -> n
lazy_signed bytes
  | Lazy.null bytes          =  0
  | Lazy.head bytes == '-'   =  fold negative 0 (Lazy.tail bytes)
  | Lazy.head bytes == '+'   =  fold positive 0 (Lazy.tail bytes)
  | otherwise                =  fold positive 0 bytes
 where
  fold                       =  Lazy.foldlChunks . foldl'


strict_unsigned             ::  (Num n) => ByteString -> n
strict_unsigned              =  foldl' positive 0

strict_signed               ::  (Num n) => ByteString -> n
strict_signed bytes
  | null bytes               =  0
  | head bytes == '-'        =  foldl' negative 0 (tail bytes)
  | head bytes == '+'        =  foldl' positive 0 (tail bytes)
  | otherwise                =  foldl' positive 0 bytes


positive                    ::  (Num n) => n -> Word8 -> n
positive  acc  byte          =  (acc * 10) + fromIntegral (byte - c2w '0')

negative                    ::  (Num n) => n -> Word8 -> n
negative  acc  byte          =  (acc * 10) - fromIntegral (byte - c2w '0')



