#!/usr/bin/env runhaskell

{-# LANGUAGE BangPatterns
  #-}

{-| The Sphere Online Judge is a collection of problems. One problem, problem
    450, came up on the mailing list as a sensible benchmark for fast integer
    parsing.
 -}


{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  https://www.spoj.pl/problems/INTEST/ 

  Slow IO?
  http://www.nabble.com/Slow-IO--td25210251.html

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


import Data.List
import Prelude hiding (drop, take)
import System.IO (stdin, stdout, stderr, Handle, putStrLn)
import Control.Monad

import Data.ByteString hiding (putStrLn, foldl')
import Data.ByteString.Nums.Careless




main                         =  do
  (n, k)                    <-  breakByte 0x20 `fmap` hGetLine stdin
  count                     <-  show `fmap` obvious (int n) (int k)
  putStrLn count


obvious n k                  =  sum `fmap` sequence (numbers n stdin)
 where
  sum                        =  foldl' (check k) 0


numbers                     ::  Int -> Handle -> [IO Int]
numbers n h                  =  const (int `fmap` hGetLine h) `fmap` [1..n]


check                       ::  Int -> Int -> Int -> Int
check k acc n
  | n `mod` k == 0           =  acc + 1
  | otherwise                =  acc


