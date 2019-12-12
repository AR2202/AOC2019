module Day10
  (pointToPoint,
  Coord(..),
  normalize,
  distanceToAll,
  detectable,
  detectableAll,
  list,
  a9,
  a4,
  a5,
  a6,
  maxDetectable,
  day10a
  )
  where
import Data.Ratio
import Data.List
import Control.Monad

--some types for Coordinate Points
data Coord = Coord {x::Point, y::Point} deriving (Show, Eq)
type Point = Rational

--data for first example
a1 = Coord 1 0
a2 = Coord 4 0
a3 = Coord 0 2
a4 = Coord 1 2
a5 = Coord 2 2
a6 = Coord 3 2
a7 = Coord 4 2
a8 = Coord 4 3
a9 = Coord 3 4
a10 = Coord 4 4

list = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]

-- functions for counting asteroids

pointToPoint :: Coord -> Coord -> Coord
pointToPoint coord1 coord2 = Coord (x coord2 - x coord1) (y coord2 - y coord1)

normalize :: Coord -> Coord
normalize (Coord 0 y)
  |y<0 = Coord 0 (-1)
  |y>0 = Coord 0 1
  |y==0 = Coord 0 0
normalize (Coord x 0)
  |x<0 = Coord (-1) 0
  |x>0 = Coord 1 0
normalize coord
  |(y coord )>0 = Coord 1 (y coord/x coord)
  |(y coord)<0 = Coord (-1) (y coord/x coord)


distanceToAll :: [Coord] -> Coord ->[Coord]
distanceToAll [] _ = []
distanceToAll xs x = map (pointToPoint x) $ filter (/=x) xs

detectable :: [Coord] -> Coord -> Int
detectable list coord = length.nub $ map normalize $ distanceToAll list coord

detectableAll :: [Coord] -> [Int]
detectableAll list = map (detectable list) list

maxDetectable list = maximum $ detectableAll list

--parsing the input
toCoords _ _ [] list = list
toCoords x y (z:zs) list
  |z == '#' = toCoords (x+1) y zs ((Coord x y ):list)
  |otherwise = toCoords (x+1) y zs list

toCoordsy _ [] list = []
toCoordsy y (z:zs) list = (toCoords 0 y z list):(toCoordsy (y+1) zs list)

--solution Part 1
day10a = do
  input <-readFile "./input/day10.txt"
  let tolines = lines input
  let sizey = length tolines
  let sizex = length $ head tolines
  let allcoords = toCoordsy 0 tolines []
  let result = maxDetectable $ join allcoords
  print result
