

module Main where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Nums.Careless -- from bytestring-nums package

bint :: B.ByteString -> Int
bint = int

main = do
 line : rest <- B.split 10 `fmap` B.getContents
 let [n, k] = map int . B.split 32 $ line
 putStrLn . show . length . tail . filter ((==0).(`mod`k).bint)  $ rest



{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
---------- Forwarded message ----------  
From: Eugene Kirpichov <ekirpichov@gmail.com>
Date: 2009/08/30
Subject: Re: [Haskell-cafe] Slow IO?
To: Steve <stevech1097@yahoo.com.au>
Cc: haskell-cafe@haskell.org


module Main where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Nums.Careless -- from bytestring-nums package

bint :: B.ByteString -> Int
bint = int

main = do
 line : rest <- B.split 10 `fmap` B.getContents
 let [n, k] = map int . B.split 32 $ line
 putStrLn . show . length . tail . filter ((==0).(`mod`k).bint)  $ rest

This does a 100MB file in 2.7s (probably because the file is cached by
the filesystem).

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

