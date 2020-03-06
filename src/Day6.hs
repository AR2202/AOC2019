module Day6
  (examplegraph,
   examplegraph2,
   nodefromvertex1,
   vertexfromkey1,
   parseInput,
   allConnectedVertices,
   day6a,
   numReachable,
   commonReachable,
   distanceToCommon,
   day6b
  )
  where


import Data.Graph
import Data.Maybe
import Data.List

--Data from the examples provided on the website

(examplegraph, nodefromvertex1,vertexfromkey1) = graphFromEdges [("B",'B',['0']),("G",'G',['B']),("H",'H',['G']),("C",'C',['B']),("D",'D',['C']),("I",'I',['D']),("E",'E',['D']),("F",'F',['E']),("J",'J',['E']),("K",'K',['J']),("L",'L',['K']),("COM",'0',[])]


(examplegraph2, nodefromvertex2,vertexfromkey2) = graphFromEdges [("B","BA",["0A"]),("G","GA",["BA"]),("H","HA",["GA"]),("C","CA",["BA"]),("D","DA",["CA"]),("I","IA",["DA"]),("E","EA",["DA"]),("F","F",["EA"]),("J","J",["EA"]),("K","KA",["JA"]),("L","LA",["KA"]),("COM","0A",[])]




allConnectedVertices graph = sum $map ((flip (-)1).length.(reachable graph)) $ vertices graph
allConnectedVertices2 graph = sum $map (length.(reachable graph)) $ vertices graph

--very basic parsing - needs to be improved

parseInput (a:c:d:')':b) = (b,b,(pure(a:c:d:[])))

--Solution to Part 1

day6a = do
  input6 <-readFile "./input/day6.txt"
  let inputs = lines input6
  let orbits = map parseInput inputs
  let (graph, nodefromvertex,vertexfromkey)=graphFromEdges orbits
  let result = allConnectedVertices2 graph
  print graph
  print result
  
--Part2

numReachable graph key vfk=  reachable graph $fromJust$vfk key


commonReachable graph key1 key2 vfk= intersect (numReachable graph key1 vfk) (numReachable graph key2 vfk)

distanceToCommon graph key1 key2 vfk= ( length (numReachable graph key1 vfk) - common) + (length (numReachable graph key2 vfk) - common)
  where common = length $ commonReachable graph key1 key2 vfk


day6b = do
  input6 <-readFile "./input/day6.txt"
  let inputs = lines input6
  let orbits = map parseInput inputs
  let (graph, nodefromvertex,vertexfromkey)=graphFromEdges orbits
  let result = (distanceToCommon graph "YOU" "SAN" vertexfromkey)-2
  print result
  
