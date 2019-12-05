module Day5
  (Indexedlist,
   execute',
   day5a
   
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

multiplied' index list = updateAtIndex (valueAtIndex (index+3) list) (((fromMode (getModeP1 (valueAtIndex index list))) (index+1) list) * ((fromMode (getModeP2 (valueAtIndex index list))) (index + 2) list)) list

added'     index list = updateAtIndex (valueAtIndex (index+3) list) (((fromMode (getModeP1 (valueAtIndex index list))) (index+1) list) + ((fromMode (getModeP2 (valueAtIndex index list)) )(index + 2) list)) list

fromMode 0 = valueAtIndexIndex

fromMode 1 = valueAtIndex

-- the main function that 'executes' the 'program'
execute' :: Int -> Indexedlist -> [Int]->[Int]
execute' index list outputs
  |getInstruction (valueAtIndex index list) == 99 = outputs
  |getInstruction (valueAtIndex index list) == 1 = execute' (index+4)( added' index list) outputs
  |getInstruction (valueAtIndex index list) == 2 = execute' (index+4) ( multiplied' index list) outputs
  |getInstruction (valueAtIndex index list) == 4 = execute' (index+2) list ((output index list):outputs) --needs to output something
  

output index list = fromMode (getModeP1 (valueAtIndex index list)) (index+1) list
--new day 5

getOptcode optcode = undefined

getModeP3 optcode = optcode `div` 10000

getModeP2 optcode = (optcode - 10000*getModeP3 optcode) `div` 1000

getModeP1 optcode = (optcode -10000*getModeP3 optcode -1000*getModeP2 optcode) `div` 100

getInstruction optcode = optcode - 10000*getModeP3 optcode-1000*getModeP2 optcode-100*getModeP1 optcode


--setting the initial values
setInput :: Int-> Indexedlist -> Indexedlist
setInput input indlist = updateAtIndex (valueAtIndex 1 indlist) input indlist 

readInt :: String -> Int
readInt = read

-- reading the input into an Indexedlist

puzzleInput5 :: IO (Indexedlist)
puzzleInput5 = do  
  input2 <- readFile "./input/day5.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  
  return indexedinput

-- Solution Part1

day5a :: IO()
day5a = do
  input5<-puzzleInput5
  let progstart = setInput 1 input5
  let outputs = execute' 2 progstart []
  print outputs
