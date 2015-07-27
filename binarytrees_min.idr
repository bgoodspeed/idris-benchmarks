--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart in Haskell
--
-- Ported to Idris by Ben Goodspeed
import System
import Data.Bits

--
-- an artificially strict tree. 
--
-- normally you would ensure the branches are lazy, but this benchmark
-- requires strict allocation.
--

-- TODONOTE Strictness annotations !Type in haskell are not required in Idris, since we are eager by default
data Tree = Nil | Node Int Tree Tree

minN : Int
minN = 4

-- TODONOTE we don't have printf, so we need to build strings and emit them using putStrLn
--io s n t = printf "%s of depth %d\t check: %d\n" s n t
io : String -> Int -> Int -> IO' ffi ()
io s n t = putStrLn (s ++ " of depth " ++ (show n) ++ "\t check: " ++ (show t))


-- traverse the tree, counting up the nodes
check : Tree -> Int
check Nil          = 0
check (Node i l r) = i + check l - check r



-- build a tree
make : Int -> Int -> Tree
make i 0 = Node i Nil Nil
make i d = let i2 = 2*i in 
               let d2 = d - 1 in
                  Node i (make (i2-1) d2) (make i2 d2)


-- allocate and check lots of trees
sumT : Int -> Int -> Int -> Int
sumT d 0 t = t
sumT  d i t = sumT d (i-1) (t + a + b)
  where a = check (make i    d)
        b = check (make (-i) d)



-- TODONOTE where clauses in haskell are often used like a postfix let clause (which idris doesn't like)
-- generate many trees
-- TODONOTE the Bits type is a bit awkward to use, we only need to pretend we have a value with a given bit set and the rest cleared
bit : Int -> Int 
bit a = pow 2 (cast a)

depth : Int -> Int -> List (Int,Int,Int)
depth d m = case (d <= m) of
                 False => []
                 True  => let n = bit (m - d + minN) 
                              s = sumT d n 0
                              rest = depth (d+2) m in
                              ((2*n, d, s) :: rest )

-- TODONOTE we don't have the MapM_ function, according to Hoogle it is equal to:
-- mapM_ : (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ : Monad m => (a -> m b) -> List a -> m ()
mapM_ f  = sequence_ . map f

createTrees : Int -> IO' ffi ()
createTrees n = let maxN     = max (minN + 2) n
                    stretchN = maxN + 1
                    c        = check (make 0 stretchN) 
                    vs       = depth minN maxN
                    long   = make 0 maxN in
                      do
                        io "stretch tree" stretchN c
--                        mapM_ (\((m,d,i)) => show m ++ "asdf") vs
                        io "long lived tree" maxN (check long)
                    

-- io (show m ++ "\t trees") d i) vs

main : IO ()
main = do
  args <- getArgs
  case args of
    [self]  => putStrLn ("usage: " ++ self ++ " <n>")
    [_, ns] => createTrees (cast ns)



