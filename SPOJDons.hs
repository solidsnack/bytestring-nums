
{-# LANGUAGE BangPatterns
  #-}


 {---------------------------- Forwarded message -----------------------------

From: d...@cse.unsw.edu.au (Donald Bruce Stewart)
Date: Jun 7 2007, 6:43 pm
Subject: Fast number parsing with strict bytestrings [Was: Re: Seemingly
subtle change causes large performance variation]
To: fa.haskell

[...snip...]

*** Solution 3: 4x faster by processing strict cache chunks

Now the fun part.

The following code is the fastest way I know to process lists of numbers
(in any language). Its' based on similar code I wrote for the language
shootout.  The key trick is to use lazy bytestrings *only* as a method
for filling the cache with newline-aligned chunks of numbers. Once
you've got that perfectly-sized chunk, walk its lines, and process them.
This is all done in Haskell, and relies on an understanding of the low
level details of bytestring optimisations.

The general framework could be reused for any code that needs to process
a list of numbers in a file, where you care about speed.

It performs as follows:

    $ time ./F < in
    29359
    ./F < in  0.24s user 0.01s system 76% cpu 0.327 total

Pretty fast..

[...snip...]

  ---------------------------- Forwarded message -----------------------------}


import Data.Char
import Data.Maybe
import Data.ByteString hiding (last, uncons)
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L

main = do
    ss <- L.getContents -- done with IO now.
    let (l,ls) = L.break (=='\n') ss
        -- don't need count, we're allocating lazily
        k      = fst . fromJust . L.readInt . last . L.split ' ' $ l
        file   = L.toChunks (L.tail ls) -- a lazy list of strict cache chunks
    print $ process k 0 file

divisibleBy :: Int -> Int -> Bool
a `divisibleBy` n = a `rem` n == 0

-- ---------------------------------------------------------------------
--
-- Optimised parsing of strict bytestrings representing \n separated numbers
--

--
-- we have the file as a list of cache chunks
-- align them on \n boundaries, and process each chunk separately
-- when the next chunk is demanded, it will be read in.
--
process :: Int -> Int -> [S.ByteString] -> Int
process k i []      = i
process k !i (s:t:ts) | S.last s /= '\n' = process k (add k i s') ts'
  where
    (s',r) = S.breakEnd (=='\n') s
    ts'    = (S.append r t) : ts        -- join chunks on line boundaries

process k i (s: ss) = process k (add k i s) ss

--
-- process a single cache-sized chunk of numbers, \n aligned
--
add :: Int -> Int -> S.ByteString -> Int
add k i s | S.null s  = i
          | otherwise = test k i (parse x) xs
  where (x,xs) = uncons s

--
-- process a single line, until \n
--
test :: Int -> Int -> Int -> ByteString -> Int
test k i !n t
    | y == '\n' = -- done reading the line, process it:
        if n `divisibleBy` k then add k (i+1) ys
                             else add k i     ys
    | otherwise = test k i n' ys
  where (y,ys) = uncons t
        n'     = parse y + 10 * n

parse c  = ord c - ord '0'

-- fastest way to take the head of a strict bytestring
uncons s = (w2c (unsafeHead s), unsafeTail s) 



