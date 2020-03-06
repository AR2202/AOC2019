{-# LANGUAGE NoMonomorphismRestriction #-}
module Day8
  (day8a
  )
  where

import Data.List.Split
import Data.String.Utils (rstrip)


layers  = chunksOf 150 

zeros  = map (length.(filter (==0))) . layers 

minZeroPos list = length $ head$ splitWhen (==(minimum (zeros list))) $ zeros list

minLayer list = head $ take 1 $ drop (minZeroPos list) $ layers list

ones  = length.(filter (==1)) . minLayer 

twos  = length.(filter (==2)) . minLayer 

readInt :: String -> Int
readInt = read
day8a = do
  input <-readFile "./input/day8.txt"
  let inputs = chunksOf 1 $ rstrip input
  let digits = map readInt inputs
  let result = (ones digits) * (twos digits)
  print result
  
