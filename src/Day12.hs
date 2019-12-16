module Day12
  (allToAllGravity,
   moonlist,
   step,
   nsteps,
   moonlist',
   sumEnergy
  )
  where

-- types for holding x,y,z data

data Pos = Pos {x::Int, y::Int, z::Int} deriving (Show,Read,Eq)
data Moon =Moon {pos::Pos, vel::Pos} deriving (Show,Read,Eq)

--data of first example

pos1 = Pos (-1) 0 2
pos2 = Pos 2 (-10) (-7)
pos3 = Pos 4 (-8) 8
pos4 = Pos 3 5 (-1)

vel1 = Pos 0 0 0
vel2 = Pos 0 0 0
vel3 = Pos 0 0 0
vel4 = Pos 0 0 0




moon1 = Moon pos1 vel1
moon2 = Moon pos2 vel2
moon3 = Moon pos3 vel3
moon4 = Moon pos4 vel4

moonlist =[moon1,moon2,moon3,moon4]


--input data

pos1' = Pos 3 3 0 
pos2' = Pos 4 (-16) 2
pos3' = Pos (-10) (-6) 5
pos4' = Pos (-3) 0 (-13)



moon1' = Moon pos1' vel1
moon2' = Moon pos2' vel2
moon3' = Moon pos3' vel3
moon4' = Moon pos4' vel4

moonlist' =[moon1',moon2',moon3',moon4']
--functions

gravity (Moon pos2 vel2)  (Moon pos1 vel1) = Moon pos1 (Pos (x vel1 + compareOne (x pos1) (x pos2)) (y vel1 + compareOne (y pos1) (y pos2)) (z vel1 + compareOne (z pos1) (z pos2)))

oneToAllGravity moon list = map (gravity moon) list

allToAllGravity list = foldl (\acc x -> oneToAllGravity x acc) list list

compareOne int1 int2
  |int1<int2 =1
  |int1==int2 = 0
  |int1>int2 = -1

velocity (Moon pos vel) = Moon (Pos (x pos + x vel) (y pos + y vel) (z pos + z vel)) vel

allVelocity list = map velocity list

step list = allVelocity $ allToAllGravity list


nsteps n list = iterate step list !! n

kinEnergy (Moon pos vel) = abs (x vel) + abs (y vel) + abs (z vel)

potEnergy (Moon pos vel) = abs (x pos) + abs (y pos) + abs (z pos)

totEnergy moon = kinEnergy moon * potEnergy moon

totEnergyAll list = map totEnergy list

sumEnergy  = sum . totEnergyAll
