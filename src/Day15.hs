module Day15
  (Indexedlist,
   
  
  
   allReachable,
   addAllDir,
   day15a
   
  )
  where


--This intcode computer programm was built up over several days and functionality has successively been added to it with every puzzle. some of the functions are not used in every puzzle.


import Data.List.Split
import Data.String.Utils
import Data.List
import Control.Monad.State

takeStep :: Int -> State Indexedlist Int
takeStep = undefined

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
--some helper functions for the moveRobot function

multiplied'' relbase index list = updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) * ((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index + 2) list)) list

added''     relbase index list = updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) + ((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index + 2) list)) list

fromMode' relbase 0 = valueAtIndexIndex

fromMode' relbase 1 = valueAtIndex

fromMode' relbase 2 = valueAtRelIndexIndex relbase

--new Day 15

nextField (x,y) 1 = (x,y+1)
nextField (x,y) 2 = (x,y-1)
nextField (x,y) 3 = (x-1,y)
nextField (x,y) 4 = (x+1,y)

fieldReached point list = foldl' nextField point list

fieldReachedFromStart list = fieldReached (0,0) list

allReachable :: (Int,Int)->[(Int,Int)]
allReachable (x,y) = filter (/= (x,y)) [(a,b)|a<-[x,(x-1),(x+1)],b<-[y,(y+1),(y-1)]]

allReachable' :: [(Int,Int)]->[(Int,Int)]
allReachable' list = list >>= allReachable

--adds all directions to each list of directions

addAllDir :: [[Integer]]->[[Integer]]
addAllDir lists = (:) <$> [1,2,3,4]<*> lists

--determins if the oxygen tank was found, if not, calls the function again with all directions added to each of the lists of inputs, after filtering out those that returned 0 (a wall) and any duplicates


moveAllDir program visited currentlyAt lists
  |any (==2) $ map (moveRobot 0 program 0) lists = length $ head lists
  |otherwise = moveAllDir program (visited ++ (allReachable' currentlyAt)) (allReachable' currentlyAt) $  filter (\l-> (moveRobot 0 program 0 l) /=0) $ addAllDir $ filter (\x-> (fieldReachedFromStart x) `notElem` visited) lists
  
-- the main function that 'executes' the 'program'
moveRobot :: Integer -> Indexedlist ->Integer->[Integer]->Integer
moveRobot index list relbase inputs
  
  |getInstruction (valueAtIndex index list) == 1 = moveRobot (index+4)( added'' relbase index list)  relbase inputs 
  |getInstruction (valueAtIndex index list) == 2 = moveRobot (index+4) ( multiplied'' relbase index list) relbase inputs 
  |getInstruction (valueAtIndex index list) == 4 && inputs == [] = output' relbase index list
  |getInstruction (valueAtIndex index list) == 4 = moveRobot (index+2) list relbase inputs 
  |getInstruction (valueAtIndex index list) == 3 = moveRobot (index+2) (setInput'' relbase (getModeP1 (valueAtIndex index list)) (head inputs) (valueAtIndex (index+1) list) list )  relbase (tail inputs) 
  |(getInstruction (valueAtIndex index list) == 5) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) == 0) = moveRobot (index+3) list relbase  inputs 
  |getInstruction (valueAtIndex index list) == 5 = moveRobot (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  relbase inputs 
  |(getInstruction (valueAtIndex index list) == 6) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) /= 0) = moveRobot (index+3) list relbase  inputs 
  |getInstruction (valueAtIndex index list) == 6 = moveRobot (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  relbase inputs
  |(getInstruction (valueAtIndex index list) == 7) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)<((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) relbase  inputs 
  |getInstruction (valueAtIndex index list) == 7 = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 0 list) relbase  inputs 
  |(getInstruction (valueAtIndex index list) == 8) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1)list)==((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) relbase inputs 
  |getInstruction (valueAtIndex index list) == 8= moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list))(valueAtIndex (index+3) list) 0 list) relbase inputs 
  |getInstruction (valueAtIndex index list) == 9 = moveRobot (index+2) list   (relbase + ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)) inputs
  




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








puzzleInput15 :: IO (Indexedlist)
puzzleInput15 = do  
  input2 <- readFile "./input/day15.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let extendedInputlist = inputlist ++ (repeat 0)
  let indexedinput = codeToIndexedlist extendedInputlist
  
  return indexedinput


-- Solution Part1



day15a:: IO ()
day15a= do
  input <-puzzleInput15
  let output = moveAllDir input [] [(0,0)] $ addAllDir [[]]

   
  print output
