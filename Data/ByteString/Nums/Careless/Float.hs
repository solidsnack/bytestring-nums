

{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
  #-}


module Data.ByteString.Nums.Careless.Float where


import Data.Char
import Prelude hiding (splitAt)
import Data.ByteString.Char8 hiding (length, inits, elem, last)
import qualified Data.ByteString.Lazy.Char8 as Lazy


import Data.ByteString.Nums.Careless.Int





{-| Types that can be read from floating point strings. The fractional part is
    taken to be the last group of digits behind a decimal point or comma.
    Characters are not decimal digits are simply skipped.
 -}
class (Intable b f, Fractional f) => Floatable b f where
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


strict_float bytes           =  case findIndices (`elem` ".,") bytes of
  [ ]                       ->  int bytes
  idx                       ->  hi' + (int lo * (0.1 ^ length digits) * s)
   where
    (hi, lo)                 =  splitAt (last idx) bytes
    hi'                      =  int hi
    s                        =  signum hi'
    digits                   =  findIndices isDigit lo

lazy_float bytes             =  case Lazy.findIndices (`elem` ".,") bytes of
  [ ]                       ->  int bytes
  idx                       ->  hi' + (int lo * (0.1 ^ length digits) * s)
   where
    (hi, lo)                 =  Lazy.splitAt (last idx) bytes
    hi'                      =  int hi
    s                        =  signum hi'
    digits                   =  Lazy.findIndices isDigit lo


