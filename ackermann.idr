-- http://shootout.alioth.debian.org/
-- shortened by Bryn Keller, Einar Karttunen and Don Stewart
-- Ported to Idris by Ben Goodspeed
module Main

-- TODONOTE getArgs handling is a bit different, read doesn't exist in the io monad so we pattern match
import System

ack : Int -> Int -> Int
ack  0 n = n+1
ack  m n = ack (m-1) $ if n == 0 then 1 else ack m (n-1)

-- TODONOTE pattern matching on array return value of getArgs seems like the default, need to use cast to convert string to int.
main : IO()
main = do
    args <- getArgs
    case args of
      [self ] => putStrLn ("usage: " ++ self ++ " <n>")
      [_, ns] => putStrLn ("Ack(3," ++ ns ++ "): " ++ show (ack 3 (cast ns)))

