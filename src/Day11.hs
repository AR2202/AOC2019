module Day11
  (Indexedlist,
   execute',
   day11a,
   day11b,
   getInputPos,
   --example1,
   inputpos1,
   inputpos2,
   --example1a,
   --example1b
   
  )
  where


--This intcode computer programm was built up over several days and functionality has successively been added to it with every puzzle. some of the functions are not used in every puzzle.


import Data.List.Split
import Data.String.Utils
import Data.List

-- The task is heavily based on indexing and updating mutable data structures - which is not how functional programming usually deals with data

-- Here, the list to be indexed into is modelled as a list of pairs of (val,index).
--Day11 types

--some types for Coordinate Points
data Position = Coord {x::Point, y::Point} deriving (Show, Eq, Read)
data PosOri = PosOri {pos :: Position, ori :: Orientation} deriving (Show, Eq, Read)
data Orientation = Up|Right_|Down|Left_ deriving (Show, Eq, Read)
type Point = Int
data Color = Black|White deriving (Show, Eq, Read)
data Panel = Panel {posori :: PosOri, color :: Color}
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



-- Day 11 new

--accessing function for points

updateX (Coord _ y) value = Coord value y

updateY (Coord x _) value = Coord x value

addToX (Coord x y) value = Coord (x+value) y

addToY (Coord x y) value = Coord x (y+value)

updateOri (PosOri pos ori) new_ori = PosOri pos new_ori

nextLeft Up = Left_
nextLeft Left_ = Down
nextLeft Down = Right_
nextLeft Right_ = Up


nextRight Up = Right_
nextRight Left_ = Up
nextRight Down = Left_
nextRight Right_ = Down


moveForward pos Up = addToY pos 1 
moveForward pos Down = addToY pos (-1)
moveForward pos Right_ = addToX pos 1
moveForward pos Left_ = addToX pos (-1)

turnLeft (PosOri pos ori) = PosOri (moveForward pos (nextLeft ori)) (nextLeft ori)

turnRight (PosOri pos ori) = PosOri (moveForward pos (nextRight ori)) (nextRight ori)

matchCoords pos2 (Panel (PosOri pos1 ori1) _)
  |pos1 == pos2 = True
  |otherwise = False

getPanelColor panelcoord paintedList
  |(pos panelcoord) `elem` (map (pos . posori) paintedList) = color $ head$filter (matchCoords (pos panelcoord)) paintedList
  |otherwise = Black

getPanelInput panelcoord paintedList
  |(getPanelColor panelcoord paintedList) == Black = 0
  |otherwise = 1

addToPainted panelcoord color paintedList = (Panel panelcoord color):paintedList

toColor 1 = White
toColor 0 = Black

turn 0 = turnLeft
turn 1 = turnRight


executeAndPaint index list currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 99 = paintedPanels
  |getInstruction (valueAtIndex index list) == 1 = executeAndPaint (index+4)( added'' relbase index list)  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 2 = executeAndPaint (index+4) ( multiplied'' relbase index list)  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 4 && paintingstate == True = executeAndPaint (index+2) list  currentPanel relbase (addToPainted currentPanel (toColor (output' relbase index list))paintedPanels) False
  |getInstruction (valueAtIndex index list) == 4 && paintingstate == False = executeAndPaint (index+2) list ((turn (output' relbase index list)) currentPanel) relbase paintedPanels True
  |getInstruction (valueAtIndex index list) == 3 = executeAndPaint (index+2) (setInput'' relbase (getModeP1 (valueAtIndex index list)) (getPanelInput currentPanel paintedPanels) (valueAtIndex (index+1) list) list )  currentPanel relbase paintedPanels paintingstate
  |(getInstruction (valueAtIndex index list) == 5) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) == 0) = executeAndPaint (index+3) list  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 5 = executeAndPaint (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  currentPanel relbase paintedPanels paintingstate
  |(getInstruction (valueAtIndex index list) == 6) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) /= 0) = executeAndPaint (index+3) list  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 6 = executeAndPaint (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  currentPanel relbase paintedPanels paintingstate
  |(getInstruction (valueAtIndex index list) == 7) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)<((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = executeAndPaint (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list)  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 7 = executeAndPaint (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 0 list)  currentPanel relbase paintedPanels paintingstate
  |(getInstruction (valueAtIndex index list) == 8) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1)list)==((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = executeAndPaint (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list)  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 8= executeAndPaint (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list))(valueAtIndex (index+3) list) 0 list)  currentPanel relbase paintedPanels paintingstate
  |getInstruction (valueAtIndex index list) == 9 = executeAndPaint (index+2) list  currentPanel (relbase + ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)) paintedPanels paintingstate
  

puzzleInput11 :: IO (Indexedlist)
puzzleInput11 = do  
  input2 <- readFile "./input/day11.txt"
  let inputs = splitOn "," $ rstrip input2
  let inputlist = map readInt inputs
  let extendedInputlist = inputlist ++ (repeat 0)
  let indexedinput = codeToIndexedlist extendedInputlist
  
  return indexedinput


-- Solution Part1



day11a = do
  input11 <- puzzleInput11
  let painted = executeAndPaint 0 input11 (PosOri (Coord 0 0) Up) 0 [] True
  let paintedcoords = map (pos.posori) painted
  let paintedatleastonce = length $ nub paintedcoords
  
  print paintedatleastonce

day11b = do
  input9 <- puzzleInput9
  let outputs = execute' 0 input9 [] [2] 0
  print outputs
