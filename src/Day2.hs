module Day2
  (Indexedlist,
   execute,
   updateAtIndex,
   codeToIndexedlist,
   added,
   valueAtIndex,
   valueAtIndexIndex,
   day2a,
   day2b,
   initialvallist,
   outputFinal,
   setAndExecute,
   findInitials
  )
  where





import Data.List.Split
import Data.String.Utils

-- The task is heavily based on indexing and updating mutable data structures - which is not how functional programming usually deals with data

-- Here, the list to be indexed into is modelled as a list of pairs of (val,index).

-- creating a Datatype to represent an indexed list

type Indexedlist = [(Int,Int)]

-- creating and Indexedlist of (val,index) from a list of val
  
codeToIndexedlist :: [Int]->Indexedlist
codeToIndexedlist code = zip code [0..]

-- getting the value at a specific index

valueAtIndex :: Int -> Indexedlist -> Int
valueAtIndex  index indlist = fst $ head $ filter (\x -> snd x == index) indlist

-- getting a value and treating it as an index to get another value

valueAtIndexIndex :: Int -> Indexedlist -> Int
valueAtIndexIndex index indlist = valueAtIndex (valueAtIndex index indlist) indlist

-- updating a value at an index by (making a new list)
-- probably a very inefficient implementation

updateAtIndex :: Int-> Int-> Indexedlist -> Indexedlist 
updateAtIndex index val indlist = (takeWhile (\x -> snd x <index) indlist) ++ ((val,index):(dropWhile (\x -> snd x <=index) indlist))

--some helper functions for the execute function

multiplied index list = updateAtIndex (valueAtIndex (index+3) list) ((valueAtIndexIndex (index+1) list) * (valueAtIndexIndex (index + 2) list)) list

added     index list = updateAtIndex (valueAtIndex (index+3) list) ((valueAtIndexIndex (index+1) list) + (valueAtIndexIndex (index + 2) list)) list

-- the main function that 'executes' the 'program'
execute :: Int -> Indexedlist -> Indexedlist
execute index list
  |(valueAtIndex index list) == 99 = list
  |(valueAtIndex index list) == 1 = execute (index+4)( added index list)
  |(valueAtIndex index list) == 2 = execute (index+4) ( multiplied index list)

--setting the initial values
setInitial :: (Int,Int) -> Indexedlist -> Indexedlist
setInitial (noun,verb) indlist = updateAtIndex 1 noun $ updateAtIndex 2 verb indlist

readInt :: String -> Int
readInt = read

-- reading the input into an Indexedlist

puzzleInput2 :: IO (Indexedlist)
puzzleInput2 = do  
  input2 <- readFile "./input/day2.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  
  return indexedinput

-- Solution Part1

day2a :: IO()
day2a = do  
  indexedinput <- puzzleInput2
  let result = outputFinal (12,2) indexedinput
  print result


-- Part 2

-- all possible inputs 

initialvallist :: [(Int,Int)]
initialvallist = [(noun,verb)| noun <- [0..99], verb <-[0..99]]


setAndExecute :: (Int, Int) -> Indexedlist -> Indexedlist
setAndExecute initials indlist = execute 0 $ setInitial initials indlist


outputFinal :: (Int, Int) -> Indexedlist -> Int
outputFinal initials indlist = valueAtIndex 0 $ setAndExecute initials indlist

-- finding the inputs that produce a specific output
-- very inefficient implementation, as all of them are tested until a solution is found

findInitials :: Int -> [(Int, Int)] -> Indexedlist -> (Int, Int)
findInitials result (x:xs) indlist
  |outputFinal  x indlist == result = x
  |xs ==[] = (100,100)   -- does not return the correct result, but just making sure the function will halt
  |otherwise = findInitials result xs indlist

-- Solution Part 2

day2b :: IO()
day2b = do
  indexedinput <- puzzleInput2
  let desiredResult = 19690720
  let (noun,verb) = findInitials desiredResult initialvallist indexedinput
  let result = 100 * noun + verb
  print result
