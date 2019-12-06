module Day4
  (day4a,
   day4b,
   digs,
   allIncrease,
   hasDouble
  )
  where

import Data.List

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

--choose n k = factorial n  `div ` (factorial (n-k) * factorial k)

factorial n = product [1..n]


allCodes n k = choose (n+k-1) k

codesAbove (x:[]) = 0
codesAbove (x:y:xs)
  |y<x = allCodes (10-x) (length (x:y:xs)) - choose (10-x) (length (x:y:xs))
  |y>x=  allCodes (10-(x+1)) (length (x:y:xs)) - choose (10-(x+1)) (length (x:y:xs)) + codesAbove (y:xs)
  |x==y =  allCodes (10-(x+1)) (length (x:y:xs)) - choose (10-(x+1)) (length (x:y:xs))+ allCodes (10-(x+1)) (length (y:xs)) - choose (10-(x+1)) (length (y:xs)) + codesAbove' (y:xs)

codesAbove' (x:[]) = 0
codesAbove' (x:y:xs)
  |y<x= allCodes (10-x) (length (y:xs))
  |otherwise = allCodes (10-y) (length (y:xs)) + codesAbove' (y:xs)
                                                                    
                                         
day4a= do
  let lowerbound =356261
  let upperbound = 846303
  let listlower = digs lowerbound
  let listupper = digs upperbound
  let aboveLower = codesAbove listlower
  let aboveUpper = codesAbove listupper
  let between = aboveLower - aboveUpper
  print between
  
  
--Part 2

--new approach with making the whole list of options

allNums :: [Int]
allNums = [356261..846303]

allIncrease :: [Int]->Bool
allIncrease (x:[]) = True
allIncrease (x:xs)= all (>= x) xs && allIncrease xs

hasDouble :: [Int]->Bool
hasDouble list = (filter (\x->length x ==2) $ group list) /=[]

hasMultiple :: [Int]->Bool
hasMultiple list = (filter (\x->length x >=2) $ group list) /=[]

day4b :: IO()
day4b = do
  let numlists = map digs allNums
  let codesPart1 = filter allIncrease $ filter hasMultiple numlists
  let codes = filter allIncrease $ filter hasDouble numlists
  let possibilitiesPart1 = length codesPart1
  let possibilities = length codes
  print possibilitiesPart1
  print possibilities
