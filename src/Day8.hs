module Day8
  (day8a
  )
  where

import Data.List.Split
import Data.String.Utils (rstrip)


layers list = chunksOf 150 list

zeros list = map (length.(filter (==0))) $ layers list

minZeroPos list = length $ head$ splitWhen (==(minimum (zeros list))) $ zeros list

minLayer list = head $ take 1 $ drop (minZeroPos list) $ layers list

ones list = length.(filter (==1)) $ minLayer list

twos list = length.(filter (==2)) $ minLayer list

readInt :: String -> Int
readInt = read
day8a = do
  input <-readFile "./input/day8.txt"
  let inputs = chunksOf 1 $ rstrip input
  let digits = map readInt inputs
  let result = (ones digits) * (twos digits)
  print result
  
