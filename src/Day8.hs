{-# LANGUAGE NoMonomorphismRestriction #-}
module Day8
  (day8a,
   day8b
  )
  where

import Data.List.Split
import Data.String.Utils (rstrip)
import Control.Applicative


layers  = chunksOf 150 

zeros  = map (length.(filter (==0))) . layers 

minZeroPos list = length $ head $ splitWhen (==(minimum numZeros)) $ numZeros
  where numZeros = zeros list

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
  
-- part 2

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:xs) = Just x

visiblePixel = safeHead . (dropWhile (==2))

transpose = getZipList . traverse ZipList

transposedImg = transpose . layers

-- Just a function for converting all the 1s to @ and all the 0s to _ to make the image visible
maybePixelToDisp (Just 1) = '@'
maybePixelToDisp (Just 0) = '_'
maybePixelToDisp _        = '.' --This should not happen, but just in case

day8b = do
  input <-readFile "./input/day8.txt"
  let inputs = chunksOf 1 $ rstrip input
  let digits = map readInt inputs
  let resultimg= (chunksOf 25) .(map maybePixelToDisp) . (map visiblePixel) $ transposedImg digits
  mapM_ print resultimg
