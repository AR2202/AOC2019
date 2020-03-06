{-# LANGUAGE NoMonomorphismRestriction #-}
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

-- I use the head function a lot here, which is unsafe, but I'm assuming that the puzzle inputs are valid

-- the functions to calculate the ammount needed


isConnectedFrom vertex1 (e, a, b) = vertex1 == a

isConnectedTo vertex1 (e,a,b) = vertex1 == b

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

splitEduct = splitOn " " . fst 

splitProduct = splitOn " " . snd 

makeLabel :: (String,String)->[(Int,Int)]
makeLabel tuple = [(read.head)(splitEduct tuple), (read.head) (splitProduct tuple)]

makeEduct  = head . tail . splitEduct 

makeProduct  = head . tail . splitProduct 

makeEdge tuple = (makeLabel tuple, makeProduct tuple, makeEduct tuple)

makeEdges  = map makeEdge . reaction2tuples

day14a :: IO ()
day14a = do
  input <-readFile "./input/day14.txt"
  let inputlines = lines input
  let edges' = concatMap makeEdges inputlines
  let graph1 = edges edges'
  let result = totalAmmount' "ORE" graph1
  print result
