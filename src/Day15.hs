module Day15
  (Indexedlist,  
   allReachable,
   addAllDir,
   addAllNewDir,
   day15a,
   testTakeStep,
   testTakeStep2,
   testTakeStep3,
   mainCode,
   mainSteps,
   visited,
   
   
  )
  where


--This intcode computer programm was built up over several days and functionality has successively been added to it with every puzzle. some of the functions are not used in every puzzle.


import Data.List.Split
import Data.String.Utils
import Data.List
import Control.Monad.State
import Control.Comonad


--Data Type for Comonad

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
   extract (Cons a _) = a
   duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
  
-- State Data type


data ProgState = ProgState { program :: Indexedlist, indexHead :: Integer, relativeBase :: Integer}
  deriving(Show,Read)

takeStep :: Integer -> Integer -> Integer -> (Indexedlist -> (Integer,ProgState))
takeStep input index relbase = \program ->moveRobot index program relbase input

takeFirstStep = takeStep 1 0 0

testTakeStep = do
  prog <-puzzleInput15
  
  let output = takeFirstStep prog
  print output
-- attempt of using the state monad

takeStep2 :: Integer -> State ProgState Integer
takeStep2 input = state $ \programState -> moveRobot_ programState input

moveRobot_ programState input = moveRobot (indexHead programState) (program programState) (relativeBase programState) input
      
moveRobot_list programState inputlist = moveRobot (indexHead programState) (program programState) (relativeBase programState) (head inputlist)

moveRobot_tuple (programState,inputlist) = moveRobot_list programState inputlist

--takeFirstStep2 = takeStep2 1 0 0

testTakeStep2 = do
  prog <-puzzleInput15
  let initialState = ProgState prog 0 0
  let output =runState (takeStep2 1) initialState
  print output

takeSteps = do
  res <- takeStep2 1
  if res == 2  
        then return res 
        else do
             res2 <- takeStep2 1 
            
             if res2 == 2  
               then return res2
               else do
                res3 <- takeStep2 1
                return res3
   
             
testTakeStep3 = do
  prog <-puzzleInput15
  let initialState = ProgState prog 0 0
  let output =runState takeSteps initialState
  print output -- infinite List: don't use
  


--StateT Transformer

mainCode :: IO ()
mainCode = runStateT code [1..] >> return ()
--
-- layer an infinite list of uniques over the IO monad
--

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    return ()

--
-- pop the next unique off the stack
--
pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

--an attempt using StateT Monad Transformer

runTakeStep2 :: StateT ProgState IO ()
runTakeStep2 = do
  output <- takeSteps_
  liftIO $ print output
  return ()

mainSteps :: IO ()
mainSteps = do
  prog <- puzzleInput15
  let initialState = ProgState prog 0 0
  runStateT runTakeStep2 initialState
  return ()

takeSteps_ :: StateT ProgState IO Integer
takeSteps_ = do
  res <- takeStep2_ 1
  if res == 2  
        then return 1 
        else do
             res2 <- takeStep2_ 1 
             return res2  
             if res2 == 2  
               then return 1
               else do
                res3 <- takeStep2_ 1
                return 3

takeStep2_ :: Integer -> StateT ProgState IO Integer
takeStep2_ input = state $ \programState -> moveRobot_ programState input

mainSteps_ :: IO ()
mainSteps_ = do
  prog <- puzzleInput15
  let initialState = ProgState prog 0 0
  output <-runStateT takeSteps_ initialState
  print output

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

fieldReachedFromStart_ reversedlist = (fieldReached (0,0)). reverse$ reversedlist

allReachable :: (Int,Int)->[(Int,Int)]
allReachable (x,y) = filter (/= (x,y)) [(a,b)|a<-[x,(x-1),(x+1)],b<-[y,(y+1),(y-1)]]


allReachable' :: [(Int,Int)]->[(Int,Int)]
allReachable' list = list >>= allReachable

--adds all directions to each list of directions

addAllDir :: [[Integer]]->[[Integer]]
addAllDir lists = (:) <$> [1,2,3,4]<*> lists

addAllNewDir point list visited = map (flip (:) list)$ filter (\x->(nextField point x) `notElem` visited)[1,2,3,4]

--determins if the oxygen tank was found, if not, calls the function again with all directions added to each of the lists of inputs, after filtering out those that returned 0 (a wall) and any duplicates

notvisited visited = filter (\x -> x `notElem` visited)

visited ::[Integer] -> State [(Int,Int)] [[Integer]]
visited inputlist = state $ \allVisited ->(addAllNewDir currentPoint inputlist allVisited,(notvisited allVisited (allReachable currentPoint ))++allVisited)
  where currentPoint = fieldReachedFromStart_ inputlist

runvisited input visitedpoints = runState (visited input) visitedpoints

newinputs visitedinputs programStateAndInput = zip ((repeat .fst) programStateAndInput ) (fst $ runvisited (snd programStateAndInput) visitedinputs)

newvisited visitedpoints programStateAndInput = snd$ runvisited (snd programStateAndInput) visitedpoints

moveAllDir programStatesAndInputs visitedpoints 
 |any (==2) $ map (fst . moveRobot_tuple) programStatesAndInputs = length $ snd $ head $ programStatesAndInputs
 |otherwise =  moveAllDir (list >>= (newinputs visitedpoints)) (list >>=(newvisited visitedpoints))
  where list = filter (\inp-> ((fst . moveRobot_tuple) inp)  /=0)  programStatesAndInputs

-- the main function that 'executes' the 'program'

moveRobot :: Integer -> Indexedlist ->Integer->Integer->(Integer,ProgState)
moveRobot index list relbase input
  
  |getInstruction (valueAtIndex index list) == 1 = moveRobot (index+4)( added'' relbase index list)  relbase input
  |getInstruction (valueAtIndex index list) == 2 = moveRobot (index+4) ( multiplied'' relbase index list) relbase input
  |getInstruction (valueAtIndex index list) == 4  = (output' relbase index list,ProgState list index relbase)
  |getInstruction (valueAtIndex index list) == 3 = moveRobot (index+2) (setInput'' relbase (getModeP1 (valueAtIndex index list)) input (valueAtIndex (index+1) list) list )  relbase input
  |(getInstruction (valueAtIndex index list) == 5) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) == 0) = moveRobot (index+3) list relbase  input 
  |getInstruction (valueAtIndex index list) == 5 = moveRobot (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  relbase input 
  |(getInstruction (valueAtIndex index list) == 6) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list) /= 0) = moveRobot (index+3) list relbase  input 
  |getInstruction (valueAtIndex index list) == 6 = moveRobot (fromMode' relbase (getModeP2 (valueAtIndex index list)) (index+2) list) list  relbase input
  |(getInstruction (valueAtIndex index list) == 7) && (((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)<((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) relbase  input 
  |getInstruction (valueAtIndex index list) == 7 = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 0 list) relbase  input 
  |(getInstruction (valueAtIndex index list) == 8) &&( ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1)list)==((fromMode' relbase (getModeP2 (valueAtIndex index list))) (index+2) list)) = moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list)) (valueAtIndex (index+3) list) 1 list) relbase input 
  |getInstruction (valueAtIndex index list) == 8= moveRobot (index+4) (updateAtRelIndex relbase (getModeP3 (valueAtIndex index list))(valueAtIndex (index+3) list) 0 list) relbase input 
  |getInstruction (valueAtIndex index list) == 9 = moveRobot (index+2) list   (relbase + ((fromMode' relbase (getModeP1 (valueAtIndex index list))) (index+1) list)) input
  




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
  let initialState = ProgState input 0 0
  let initialStatesAndInputs = zip (repeat initialState) [[1],[2],[3],[4]]
  let initialVisited =[(0,0)]
  let output = moveAllDir initialStatesAndInputs initialVisited

   
  print output
