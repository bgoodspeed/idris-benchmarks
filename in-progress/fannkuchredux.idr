{-  The Computer Language Benchmarks Game
    http://shootout.alioth.debian.org/
    contributed by Miha Vučkovič
-}

import System


flop' : Integer -> (List Integer, List Integer) -> (List Integer , List Integer)
flop' 0 (t, r) = (t, r)
flop' n ((h::t), r) = flop' (n-1) (t, h::r)


-- TODONOTE there was a mystery t in the last block
flop : List Integer -> List Integer
flop (2::x1::t) = x1::2::t
flop (3::x1::x2::t) = x2::x1::3::t
flop (4::x1::x2::x3::t) = x3::x2::x1::4::t
flop (5::x1::x2::x3::x4::t) = x4::x3::x2::x1::5::t
flop (6::x1::x2::x3::x4::x5::t) = x5::x4::x3::x2::x1::6::t
flop (7::x1::x2::x3::x4::x5::x6::t) = x6::x5::x4::x3::x2::x1::7::t
flop (h::t) = let	(t, r) = flop' h ((h::t), t) in
                  r

flopS : List Integer -> Integer
flopS (1::_) = 0
flopS lst = 1 + flopS (flop lst)

rotate : Integer -> List Integer -> List Integer
rotate n (h::t) = rotate' (n-1) t where
	rotate' 0 l = h::l
	rotate' n (f::t) = f::(rotate' (n-1) t)

checksum : Integer -> Integer -> Integer
checksum i f = if mod i 2 == 0 then f else -f


-- TODONOTE used seq cs $ seq sf to force them to be evaluated per strictness rules https://www.haskell.org/haskellwiki/Seq

pfold : (Integer, Integer) -> List (Integer, Integer) -> (Integer, Integer)
pfold r [] = r
pfold (ac, af) ((c, f)::t)  = let sc = ac + c
                                  sf = max af f in
                                  pfold (sc, sf) t 

--TODONOTE this is an interesting list version of this stream function
iterateN : Integer-> (Integer -> Integer) -> Integer -> List Integer
iterateN 0 f x = []
iterateN n f x = x :: iterateN (n-1) f (f x)


--TODONOTE the version of permutations given, and the standard library one from haskell don't want to typecheck (it gets confused about the interleave' return value of pairs, not sure why)
--TODONOTE however this one works well, adapted from http://rosettacode.org/wiki/Permutations#Haskell
permutations : (Eq a) => List a -> List (List a)
permutations [] = [[]] 
permutations xs = [x::ys | x <- xs, ys <- permutations (delete x xs)]

permut : Integer -> List (List Integer)
permut n = permutations [1..n]


checkFlopSum : (Integer, List Integer) -> (Integer, Integer)
checkFlopSum (i, p) = let flops = flopS p 
                          cksum = checksum i flops in 
                          (cksum, flops)

lengthAsInteger : List a -> Integer
lengthAsInteger l = fromNat (length l)





--TODONOTE this is not technically unsafe, but the idris one demands the proof that lengths match up
unsafeZip : List a -> List b -> List (a, b)
unsafeZip [] bs = []
unsafeZip as [] = []
unsafeZip (a::as) (b::bs) = (a,b) :: unsafeZip as bs

zipZeroOnto : Integer -> List (Integer, List Integer)
zipZeroOnto n = let pn = permut n
                    leftZip = [0 .. ((lengthAsInteger pn) - 1)]
                    in unsafeZip leftZip pn 

--TODONOTE call to Refl here to prove 0..(length permut n) and permut n are the same size
calcCheckSumAndFlops : Integer -> (Integer, Integer)
calcCheckSumAndFlops n = pfold (0,0) $ map checkFlopSum $ zipZeroOnto n 

buildOutput : Integer -> Integer -> Integer -> String
buildOutput n chksm mflops = (show chksm) ++ "\nPfannkuchen(" ++ (show n) ++ ") = " ++ (show $ mflops)


--TODONOTE read.head <$> getArgs is not supported
main : IO ()
main = do
   args <- getArgs
   case args of
        [self]  => putStrLn ("usage: " ++ self ++ " n")
        [_, ns] => let (chksum, mflops) = calcCheckSumAndFlops (cast ns) in
                       putStrLn $ (buildOutput (cast ns) chksum mflops)
--        [_, ns] =>  let (chksm, mflops) = pfold (0,0) $ map (\(i, p) => let flops = flopS p in (checksum i flops, flops)) $ zip [0..] (permut n)
 --                   putStrLn $ (show chksm) ++ "\nPfannkuchen(" ++ (show n) ++ ") = " ++ (show $ mflops)
