{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Day16(
  solution16a,
  day16a,
  
            )
  where

import Control.Monad
import Data.List
import Data.String.Utils (rstrip)


pattern1 :: [Int]
pattern1  = join $ repeat [0,1,0,-1]

replicateEach :: [a]->Int->[a]
replicateEach list n = join $ map (replicate n) list

replicatePattern :: Int -> [Int]
replicatePattern  = replicateEach pattern1

patterns :: [[Int]]
patterns = map tail $ map replicatePattern [1..]

zipWithx :: [Int]->[Int]->[Int]
zipWithx = zipWith (*)

multiplyWithPatterns :: [[Int]] -> [[Int]]
multiplyWithPatterns list = zipWith zipWithx patterns list

replicateList :: [Int]->[[Int]]
replicateList list = replicate (length list) list

stepFFT :: [Int] -> [Int]
stepFFT  = map (flip mod 10 . abs . sum) . multiplyWithPatterns . replicateList 


first7 :: String -> Int
first7 = read . (take 7)

toListOfStrings :: String -> [String]
toListOfStrings  = map pure

stringToDigits :: String -> [Int]
stringToDigits = map read . toListOfStrings

solution16a :: String -> [Int]
solution16a input = take 8 $ iterate stepFFT (stringToDigits input)!!100

--too much memory pressure, don't run this
solution16b :: String -> [Int]
solution16b input = take 8 $ drop (first7  input) $ iterate stepFFT (join (replicate 10000 (stringToDigits input))) !!100

day16a :: IO()
day16a = do
  input16 <- readFile "../input/day16.txt"
  putStrLn $ concat $ map show $ solution16a $ rstrip input16
