-- $Id: ary.ghc3.ghc,v 1.1 2004-05-19 18:09:16 bfulgham Exp $
-- http://www.bagley.org/~doug/shootout/
-- from Julian Assange
-- Ported to Idris by Ben Goodspeed

module Main 

-- TODONOTE in haskell you can import particular functions import Module(f1,f2) etc, we don't have that granularity in idris

import System
--import Numeric
-- UArray is an unboxed ghc extension to haskell'98
--import IArray(UArray,array,(!))

main = do
  arg <- getArgs
  case arg of
    [self]      => putStrLn ("usage: " ++ self ++ " <number>")
    [_, number] => putStrLn (show (ary (cast number)))
          
-- TODONOTE no idea what this function does.

ary : Int -> Int
ary n = x!m where
	    m = n-1
	    x : Array Int Int
	    x = array (0,m) [(i,y!i) | i <- [m,(m-1)..0]]
	    y = array (0,m) [(i,i)   | i <- [0..m]]
