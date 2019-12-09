module Day7
  (Indexedlist,
   execute',
   day7a,
   getInputPos,
   --example1,
   inputpos1,
   inputpos2,
   example1a,
   example1b
   
  )
  where





import Data.List.Split
import Data.String.Utils
import Data.List

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
execute' :: Int -> Indexedlist -> [Int]->[Int]->[Int]
execute' index list outputs inputs
  |getInstruction (valueAtIndex index list) == 99 = outputs
  |getInstruction (valueAtIndex index list) == 1 = execute' (index+4)( added' index list) outputs inputs
  |getInstruction (valueAtIndex index list) == 2 = execute' (index+4) ( multiplied' index list) outputs inputs
  |getInstruction (valueAtIndex index list) == 4 = execute' (index+2) list ((output index list):outputs) inputs --needs to output something
  |getInstruction (valueAtIndex index list) == 3 = execute' (index+2) (setInput' (head inputs) (valueAtIndex (index+1) list) list ) outputs (tail inputs)
  |(getInstruction (valueAtIndex index list) == 5) &&( ((fromMode (getModeP1 (valueAtIndex index list))) (index+1) list) == 0) = execute' (index+3) list outputs inputs
  |getInstruction (valueAtIndex index list) == 5 = execute' (fromMode (getModeP2 (valueAtIndex index list)) (index+2) list) list outputs inputs
  |(getInstruction (valueAtIndex index list) == 6) && (((fromMode (getModeP1 (valueAtIndex index list))) (index+1) list) /= 0) = execute' (index+3) list outputs inputs
  |getInstruction (valueAtIndex index list) == 6 = execute' (fromMode (getModeP2 (valueAtIndex index list)) (index+2) list) list outputs inputs
  |(getInstruction (valueAtIndex index list) == 7) && (((fromMode (getModeP1 (valueAtIndex index list))) (index+1) list)<((fromMode (getModeP2 (valueAtIndex index list))) (index+2) list)) = execute' (index+4) (updateAtIndex (valueAtIndex (index+3) list) 1 list) outputs inputs
  |getInstruction (valueAtIndex index list) == 7 = execute' (index+4) (updateAtIndex (valueAtIndex (index+3) list) 0 list) outputs inputs
  |(getInstruction (valueAtIndex index list) == 8) &&( ((fromMode (getModeP1 (valueAtIndex index list))) (index+1)list)==((fromMode (getModeP2 (valueAtIndex index list))) (index+2) list)) = execute' (index+4) (updateAtIndex (valueAtIndex (index+3) list) 1 list) outputs inputs
  |getInstruction (valueAtIndex index list) == 8= execute' (index+4) (updateAtIndex (valueAtIndex (index+3) list) 0 list) outputs inputs
  
  

runAmps indlist outputs [] = head outputs
runAmps indlist outputs sequence = runAmps indlist (execute' 0 indlist [] ((head sequence) : (head outputs) :[])) (tail sequence)

output index list = fromMode (getModeP1 (valueAtIndex index list)) (index+1) list
--new day 5

getOptcode optcode = undefined

getModeP3 optcode = optcode `div` 10000

getModeP2 optcode = (optcode - 10000*getModeP3 optcode) `div` 1000

getModeP1 optcode = (optcode -10000*getModeP3 optcode -1000*getModeP2 optcode) `div` 100

getInstruction optcode = optcode - 10000*getModeP3 optcode-1000*getModeP2 optcode-100*getModeP1 optcode


--New Day7



setInput' input1  inputpos indlist= updateAtIndex inputpos input1 indlist

setInputs input1 input2 indlist = setInput' input2 (inputpos2 indlist) $ setInput' input1  (inputpos1 indlist) indlist


  
inputpos1 indlist = valueAtIndex firstinput indlist
  where firstinput = (+1)$snd$head $ getInputPos indlist

inputpos2 indlist = valueAtIndex secondinput indlist
  where secondinput = (+1)$snd$head$tail$getInputPos indlist

getInputPos :: Indexedlist->Indexedlist
getInputPos optcode = filter (\x->fst x==3) optcode

--setting the initial values
setInput :: Int-> Indexedlist -> Indexedlist
setInput input indlist = updateAtIndex (valueAtIndex 1 indlist) input indlist 

readInt :: String -> Int
readInt = read

-- reading the input into an Indexedlist

puzzleInput7 :: IO (Indexedlist)
puzzleInput7 = do  
  input2 <- readFile "./input/day7.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  
  return indexedinput

example1Input :: IO (Indexedlist)
example1Input = do  
  input2 <- readFile "./input/day7ex1.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  
  return indexedinput

-- Solution Part1



example1a = do
  input <-example1Input
  let result = runAmps input [0] [4,3,2,1,0]
  print result
  
example1b = do
  input <-example1Input
  let results = map (runAmps input [0]) sequences
  let result = maximum results
  print result


sequences = permutations [0..4]


day7a = do
  input <-puzzleInput7
  let results = map (runAmps input [0]) sequences
  let result = maximum results
  print result


puzzleInput5 :: IO (Indexedlist)
puzzleInput5 = do  
  input2 <- readFile "./input/day5.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let indexedinput = codeToIndexedlist inputlist
  
  return indexedinput


day5a :: IO()
day5a = do
  input5<-puzzleInput5
  let progstart = setInput 1 input5
  let outputs = execute' 2 progstart [] [1]
  print outputs


day5b :: IO()
day5b = do
  input5<-puzzleInput5
  let progstart = setInput 5 input5
  let outputs = execute' 2 progstart [][5]
  print outputs

