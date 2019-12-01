module Day1
  (fuel,
   day1a
  )
  where

-- partially applying divison and subtraction functions to facilitate composition later on

div3 :: Int -> Int
div3 n = div n 3 -- div already rounds the result down to the next smaller integer

subtract2 :: Int -> Int
subtract2 n = n - 2

-- calculating fuel

fuel :: Int ->Int
fuel = subtract2 . div3


-- for Part 2:

extrafuel f
  |fuel f <= 0 = 0
  |otherwise = fuel f + extrafuel (fuel f)

-- reading the input and printing the results

readInt :: String -> Int
readInt = read



day1a :: IO()
day1a = do  -- Prints results for both part 1 and part2
  input1 <- readFile "input/day1.txt"
  let inputs = lines input1
  let masses = map readInt inputs
  let fuels = map fuel masses
  let sumFuels = sum fuels
  let extrafuels = map extrafuel fuels
  let sumExtra = sum extrafuels
  putStrLn "the sum of fuel for modules is:"
  print sumFuels
  putStrLn "The sum of fuel for modules and fuel is:"
  print $ sumExtra + sumFuels


