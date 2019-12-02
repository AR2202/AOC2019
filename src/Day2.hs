module Day2
  (Indexedlist,
   execute,
   updateAtIndex,
   codeToIndexedlist,
   added,
   valueAtIndex,
   valueAtIndexIndex,
   day2a
  )
  where





import Data.List.Split
import Data.String.Utils

type Indexedlist = [(Int,Int)]

codeToIndexedlist :: [Int]->Indexedlist
codeToIndexedlist code = zip code [0..]

valueAtIndex :: Int -> Indexedlist -> Int
valueAtIndex  index indlist = fst $ head $ filter (\x -> snd x == index) indlist

valueAtIndexIndex :: Int -> Indexedlist -> Int
valueAtIndexIndex index indlist = valueAtIndex (valueAtIndex index indlist) indlist

updateAtIndex :: Int-> Int-> Indexedlist -> Indexedlist 
updateAtIndex index val indlist = (takeWhile (\x -> snd x <index) indlist) ++ ((val,index):(dropWhile (\x -> snd x <=index) indlist))

multiplied index list = updateAtIndex (valueAtIndex (index+3) list) ((valueAtIndexIndex (index+1) list) * (valueAtIndexIndex (index + 2) list)) list

added     index list = updateAtIndex (valueAtIndex (index+3) list) ((valueAtIndexIndex (index+1) list) + (valueAtIndexIndex (index + 2) list)) list

execute index list
  |(valueAtIndex index list) == 99 = list
  |(valueAtIndex index list) == 1 = execute (index+4)( added index list)
  |(valueAtIndex index list) == 2 = execute (index+4) ( multiplied index list)



readInt :: String -> Int
readInt = read

day2a :: IO()
day2a = do  
  input2 <- readFile "./input/day2.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  let code = updateAtIndex 1 12 $ updateAtIndex 2 2 indexedinput
  let executed = execute 0 code
  let result = valueAtIndex 0 executed
 
  print result


