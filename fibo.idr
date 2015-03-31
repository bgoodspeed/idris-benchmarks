-- $Id: fibo.ghc,v 1.5 2005-04-25 19:01:38 igouy-guest Exp $
-- http://www.bagley.org/~doug/shootout/
-- ported to Idris by Ben Goodspeed

import System

fibI : Int -> Int
fibI n = case n < 2 of
             True => 1
             _    => fibI (n-2) + fibI (n-1)

fibonacci : Int -> Int
fibonacci n = if n < 2 then 1 else fibonacci (n-1) + fibonacci (n-2)



main : IO ()
main = do args <- getArgs
          case args of 
               [self] => putStrLn ("usage: " ++ self ++ " <n>")
               [_, n] => putStrLn (show (fibI (cast n))) 


