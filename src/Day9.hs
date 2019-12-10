module Day9
  (Indexedlist,
   execute',
   day9a,
   day9b,
   getInputPos,
   --example1,
   inputpos1,
   inputpos2,
   --example1a,
   --example1b
   
  )
  where





import Data.List.Split
import Data.String.Utils
import Data.List

-- The task is heavily based on indexing and updating mutable data structures - which is not how functional programming usually deals with data

-- Here, the list to be indexed into is modelled as a list of pairs of (val,index).

-- creating a Datatype to represent an indexed list

type Indexedlist = [(Integer,Integer)]

-- creating and Indexedlist of (val,index) from a list of val
  
codeToIndexedlist :: [Integer]->Indexedlist
codeToIndexedlist code = zip code [0..]

-- getting the value at a specific index

valueAtIndex :: Integer -> Indexedlist -> Integer
valueAtIndex  index indlist = fst $ head $ filter (\x -> snd x == index) indlist

  
-- getting a value and treating it as an index to get another value

valueAtIndexIndex :: Integer -> Indexedlist -> Integer
valueAtIndexIndex index indlist = valueAtIndex (valueAtIndex index indlist) indlist

valueAtRelIndexIndex relbase index indlist = valueAtIndex ((valueAtIndex index indlist)+relbase) indlist

-- updating a value at an index by (making a new list)
-- probably a very inefficient implementation

updateAtIndex :: Integer-> Integer-> Indexedlist -> Indexedlist 
updateAtIndex index val indlist = (takeWhile (\x -> snd x <index) indlist) ++ ((val,index):(dropWhile (\x -> snd x <=index) indlist))

updateAtRelIndex relbase mode index val indlist
  |mode == 0 = updateAtIndex index val indlist
  |otherwise = updateAtIndex (index+relbase) val indlist
--some helper functions for the execute function

multiplied'' relbase index list = updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) * ((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index + 2) list)) list

added''     relbase index list = updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) + ((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index + 2) list)) list

fromMode' relbase 0 = valueAtIndexIndex

fromMode' relbase 1 = valueAtIndex

fromMode' relbase 2 = valueAtRelIndexIndex relbase

-- the main function that 'executes' the 'program'
execute' :: Integer -> Indexedlist -> [Integer]->[Integer]->Integer->[Integer]
execute' index list outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 99 = outputs
  |getInstruction (valueAtIndex index list) == 1 = execute' (index+4)( added'' relbase index list) outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 2 = execute' (index+4) ( multiplied'' relbase index list) outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 4 = execute' (index+2) list ((output' relbase index list):outputs) inputs relbase --needs to output something
  |getInstruction (valueAtIndex index list) == 3 = execute' (index+2) (setInput'' relbase (getModeP1 (valueAtIndex index list)) (head inputs) (valueAtIndex (index+1) list) list ) outputs (tail inputs) relbase
  |(getInstruction (valueAtIndex index list) == 5) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) == 0) = execute' (index+3) list outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 5 = execute' (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list outputs inputs relbase
  |(getInstruction (valueAtIndex index list) == 6) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) /= 0) = execute' (index+3) list outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 6 = execute' (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list outputs inputs relbase
  |(getInstruction (valueAtIndex index list) == 7) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)<((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = execute' (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 7 = execute' (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 0 list) outputs inputs relbase
  |(getInstruction (valueAtIndex index list) == 8) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1)list)==((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = execute' (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 8= execute' (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list))(valueAtIndex (index+3) list) 0 list) outputs inputs relbase
  |getInstruction (valueAtIndex index list) == 9 = execute' (index+2) list outputs inputs (relbase + ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)) 
  




output' relbase index list = (fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list


getOptcode optcode = undefined

getModeP3 optcode = optcode `div` 10000

getModeP2 optcode = (optcode - 10000*getModeP3 optcode) `div` 1000

getModeP1 optcode = (optcode -10000*getModeP3 optcode -1000*getModeP2 optcode) `div` 100

getInstruction optcode = optcode - 10000*getModeP3 optcode-1000*getModeP2 optcode-100*getModeP1 optcode


--New Day7

setInput'' relbase mode input1 inputpos indlist =updateAtRelIndex relbase mode inputpos input1 indlist

setInput' input1  inputpos indlist= updateAtIndex inputpos input1 indlist

setInputs input1 input2 indlist = setInput' input2 (inputpos2 indlist) $ setInput' input1  (inputpos1 indlist) indlist


  
inputpos1 indlist = valueAtIndex firstinput indlist
  where firstinput = (+1)$snd$head $ getInputPos indlist

inputpos2 indlist = valueAtIndex secondinput indlist
  where secondinput = (+1)$snd$head$tail$getInputPos indlist

getInputPos :: Indexedlist->Indexedlist
getInputPos optcode = filter (\x->fst x==3) optcode

--setting the initial values
setInput :: Integer-> Indexedlist -> Indexedlist
setInput input indlist = updateAtIndex (valueAtIndex 1 indlist) input indlist 

readInt :: String -> Integer
readInt = read

-- reading the input into an Indexedlist

puzzleInput9 :: IO (Indexedlist)
puzzleInput9 = do  
  input2 <- readFile "./input/day9.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let extendedInputlist = inputlist ++ (repeat 0)
  let indexedinput = codeToIndexedlist extendedInputlist
  
  return indexedinput


-- Solution Part1



day9a = do
  input9 <- puzzleInput9
  let outputs = execute' 0 input9 [] [1] 0
  print outputs

day9b = do
  input9 <- puzzleInput9
  let outputs = execute' 0 input9 [] [2] 0
  print outputs
    

sequences = permutations [0..4]



