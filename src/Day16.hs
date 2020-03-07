{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Day16(
  solution16a
            )
  where
import Day4 (digs)
import Control.Monad
import Data.List



pattern1  = join $ repeat [0,1,0,-1]

replicateEach list n = join $ map (replicate n) list

replicatePattern  = replicateEach pattern1

patterns = map tail $ map replicatePattern [1..]

zipWithx = zipWith (*)

multiplyWithPatterns list = zipWith zipWithx patterns list

replicateList list = replicate (length list) list

stepFFT list = map (flip mod 10 . abs . sum) $ multiplyWithPatterns $ replicateList list

solution16a input = take 8 $ iterate stepFFT (digs input)!!100
