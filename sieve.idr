-- $Id: sieve.ghc,v 1.2 2004-11-23 08:08:45 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- from Roland Dowdeswell
-- adjusted by Aaron Denney, borrowing strictness attempt 
-- from Malcom Wallace's matrix multiplication
-- ported to Idris by Ben Goodspeed

module Main 

import System
-- I believe this was only done to force it to evalute the whole list, which shouldn't be an issue in Idris
force : List a -> Bool
force [] = True
force (x::xs) = force xs



strictlast : List (List a) -> List a
strictlast [x] = x
strictlast (x::xs) = let f =  force x in  strictlast xs

-- we use Int rather than let Haskell default to Integer,
-- because we are trying to remain competetive with other
-- languages that do not do arbitrary precision math by
-- default...
sieve : List Int -> List Int
sieve [] = []
sieve (h::t) = h :: sieve [x| x<-t, (x `mod` h) /= 0]


-- Here we try to force it to recompute at each step.
mytest : Int -> Int
mytest n = cast $ length $ strictlast $ map sieve $ replicate (cast n) ([2..8192])


main : IO ()
main = do 
  args <- getArgs
  case args of 
       [self] => putStrLn ("usage: " ++ self ++ " <n>")
       [_, n] => putStrLn . ("Count: "++) . show . mytest $ cast n
