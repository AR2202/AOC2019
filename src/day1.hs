module Day1
  (fuel,
   day1a
  )
  where

div3 :: Int -> Int
div3 n = div n 3

subtract2 :: Int -> Int
subtract2 n = n - 2

fuel :: Int ->Int
fuel = subtract2 . div3

readInt :: String -> Int
readInt = read

extrafuel f
  |fuel f <= 0 = 0
  |otherwise = fuel f + extrafuel (fuel f)

day1a :: IO()
day1a = do
  input1 <- readFile "../input/day1.txt"
  let inputs = lines input1
  let masses = map readInt inputs
  let fuels = map fuel masses
  let sumFuels = sum fuels
  let extrafuels = map extrafuel fuels
  let sumExtra = sum extrafuels
  putStrLn "the sum of Fuel for moduels is"
  print sumFuels
  putStrLn "The sum of Fuel for modules and fuel is"
  print $ sumExtra + sumFuels


