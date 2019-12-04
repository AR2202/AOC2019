module Day4
  (day4a,
   digs
  )
  where

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
  
  
        
