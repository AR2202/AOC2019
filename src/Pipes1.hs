module Pipes1
  (runGreet
  )
  where

import Pipes
import Pipes.Core
import Control.Monad

stdin :: Producer String IO r
stdin = forever $ lift getLine >>= yield

greet :: String -> IO ()
greet = putStrLn . ("Hello " ++)

greetStdin :: Effect IO r
greetStdin = for stdin $ \name -> lift (greet name)


runGreet = runEffect greetStdin
