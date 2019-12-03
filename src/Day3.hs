module Day3
  (Coordinate(..),
   Wire,
   Distance,
   Direction (..),
   Segment(..),
   Coord(..),
   addAll,
   makeWire',
   Sign (..),
   extendWire,
   wholeWire,
   intersections,
   manhattendist,
   readDirection,
   readDistance,
   readSegment,
   day3a
   
  )
  where
import Data.List
import Data.List.Split
import Control.Monad
import Data.String.Utils (rstrip)

-- some Types

type Coordinate = (Int,Int)


data Coord = X|Y

type Wire = [Coordinate]

type Distance = Int

data Direction = R  |L |U |D
  deriving (Show, Eq, Read)

data Sign = Minus| Plus

type Segment = (Direction,Distance)



extendWire :: Wire -> Segment -> Wire
extendWire [] seg = makeWire' (0,0) seg
extendWire wire seg = makeWire' (head wire) seg

makeWire' :: Coordinate -> Segment-> Wire
makeWire' point (d,dist)
  |d == R = addAll Plus X dist point
  |d == L = addAll Minus X dist point
  |d == U = addAll Plus Y dist point
  |d == D = addAll Minus Y dist point

addAll :: Sign-> Coord -> Distance -> Coordinate -> Wire
addAll Plus X dist (x0,y0) = reverse [ (x,y0)|x<-[x0..(x0 + dist)]]
addAll Minus X dist (x0,y0) = [ (x,y0)|x<-[(x0 - dist)..x0]]
addAll Plus Y dist (x0,y0) = reverse [ (x0,y)|y<-[y0..(y0+dist)]]
addAll Minus Y dist (x0,y0) = [(x0,y)|y<-[(y0 - dist)..y0]]

wholeWire :: [Segment] -> Wire
wholeWire seglist = join $ scanl extendWire [(0,0)] seglist 

intersections :: Wire -> Wire -> Wire
intersections wire1 wire2 = filter (/= (0,0)) $ nub $ intersect wire1 wire2


manhattendist :: Coordinate -> Int
manhattendist (x,y) = abs x+abs y


mindist intersectionlist = minimum $ map manhattendist intersectionlist

day3a = do
  input3 <- readFile "./input/day3.txt"
  let wires = map (map readSegment) $ map (map (splitAt 1)) $ map (splitOn ",") $ splitOn "\n" $ rstrip input3
  let wire1 = wholeWire $ wires!!0
  let wire2 = wholeWire $ wires!!1
  let result = mindist $ intersections wire1 wire2
  print result

readDirection :: String -> Direction
readDirection = read

readDistance :: String ->Distance
readDistance = read

readSegment :: (String,String) -> Segment
readSegment (a,b)= (readDirection a, readDistance b)

