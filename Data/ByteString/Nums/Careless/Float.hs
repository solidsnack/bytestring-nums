

{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
  #-}


module Data.ByteString.Nums.Careless.Float where


import Data.Char
import Prelude hiding (break, length, null, drop, tail, head)
import Data.ByteString hiding (head, break, pack)
import Data.ByteString.Char8 hiding (inits, elem, last, foldl')
import qualified Data.ByteString.Lazy.Internal as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy


import Data.ByteString.Nums.Careless.Int





{-| Types that can be read from floating point strings. A floating point
    string is taken to be a string of digits with up to one comma or period
    mixed in with the digits.
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




strict_float bytes
  | null bytes               =  0
  | head bytes == '-'        =  foldn 0 (tail integer) + nfrac
  | head bytes == '+'        =  foldp 0 (tail integer) + pfrac
  | otherwise                =  foldp 0 integer + pfrac
 where
  foldn                      =  foldl' negative
  foldp                      =  foldl' positive
  (integer, fractional)      =  break point bytes
  fractional'                =  tail fractional
  p                          =  0.1 ^ length fractional'
  nfrac
    | null fractional        =  0
    | otherwise              =  foldn 0 fractional' * p
  pfrac
    | null fractional        =  0
    | otherwise              =  foldp 0 fractional' * p


lazy_float bytes
  | Lazy.null bytes          =  0
  | Lazy.head bytes == '-'   =  foldn 0 (Lazy.tail integer) + nfrac
  | Lazy.head bytes == '+'   =  foldp 0 (Lazy.tail integer) + pfrac
  | otherwise                =  foldp 0 integer + pfrac
 where
  foldn                      =  Lazy.foldlChunks (foldl' negative)
  foldp                      =  Lazy.foldlChunks (foldl' positive)
  (integer, fractional)      =  Lazy.break point bytes
  fractional'                =  Lazy.tail fractional
  p                          =  0.1 ^ Lazy.length fractional'
  nfrac
    | Lazy.null fractional   =  0
    | otherwise              =  foldn 0 fractional' * p
  pfrac
    | Lazy.null fractional   =  0
    | otherwise              =  foldp 0 fractional' * p


point c                      =  c == '.' || c == ','



