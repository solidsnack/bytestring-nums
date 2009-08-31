#!/usr/bin/env runhaskell

{-# LANGUAGE BangPatterns
           , ScopedTypeVariables
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

import Data.ByteString hiding (putStrLn, foldl', length, filter, take)
import Data.ByteString.Nums.Careless




main                         =  do
  (n, k)                    <-  breakByte 0x20 `fmap` hGetLine stdin
  strings                   <-  split 0x0a `fmap` hGetContents stdin
  putStrLn . show . process (int k) . ints . take (int n) $ strings
 where
  ints                      ::  [ByteString] -> [Int]
  ints                       =  fmap int
  process                   ::  Int -> [Int] -> Int
  process k                  =  length . filter (condition k)
   where
    condition k num          =  num `mod` k == 0


