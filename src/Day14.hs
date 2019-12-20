module Day14
  (totalAmmount',
   day14a,
   splitHead,
   subtosub,
   splitEducts,
   reaction2ProductsEducts,
   reaction2Components,
   reaction2tuples,
   makeLabel,
   makeEduct,
   makeProduct,
   makeEdge,
   makeEdges
   
  )
  where

import Algebra.Graph.Labelled
import Algebra.Graph.Label
import Data.Ratio
import Text.ParserCombinators.ReadP
import Data.List.Split

-- the functions to calculate the ammount needed

isConnectedFrom vertex1 (e, a, b) = vertex1 == a

isConnectedTo vertex1 (e,a,b) = vertex1 ==b

connectedFrom vertex1 graph = filter (isConnectedFrom vertex1)$edgeList graph

connectedTo vertex1 graph = filter (isConnectedTo vertex1)$edgeList graph

requiredAmmount' graph (e,a,b)  = (totalAmmount' a graph)`roundedUp` snd (head e) * fst(head e)


totalAmmount' b graph
  |(connectedTo b graph) == [] = 1
  |otherwise = sum $ map (requiredAmmount' graph) (connectedTo b graph) 


roundedUp a b
  |a `mod` b == 0 = a `div` b
  |otherwise = 1+ (a `div`b)

-- parsing the Input

splitHead :: String -> [String] -> ([String],[String])
splitHead string list = (splitOn string (head list),tail list)

splitEducts  = splitHead ", " 

subtosub x = [(a,b)|a<-(fst x), b<-(snd x)]

reaction2ProductsEducts = splitOn " => "

reaction2Components = splitEducts . reaction2ProductsEducts

reaction2tuples = subtosub . reaction2Components

splitEduct t= splitOn " " $fst t

splitProduct t= splitOn " " $snd t

makeLabel :: (String,String)->[(Int,Int)]
makeLabel tuple = (read (head(splitEduct tuple)), read(head (splitProduct tuple))):[]

makeEduct tuple = head $ tail $ splitEduct tuple

makeProduct tuple = head $ tail $ splitProduct tuple

makeEdge tuple = (makeLabel tuple, makeProduct tuple, makeEduct tuple)

makeEdges reaction = map makeEdge $ reaction2tuples reaction

day14a :: IO ()
day14a = do
  input <-readFile "./input/day14.txt"
  let inputlines = lines input
  let edges' = concat $ map makeEdges inputlines
  let graph1 = edges edges'
  let result = totalAmmount' "ORE" graph1
  print result
