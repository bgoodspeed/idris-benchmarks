--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- 
-- contributed by Jeff Newbern
-- Modified to fastest.hs by Chris Kuklewicz, 6 Jan 2006
-- Modified to fixed-fasta.hs by Chris Kuklewicz, 17 Jan 2006
-- 
-- Uses random generation code derived from Simon Marlow and Einar
-- Karttunen's "random" test entry.  No longer uses Double during run,
-- everything has been pre-converted to Int.  And pre-converted to a
-- binary tree for lookup.  Ideally this tree could be constructed
-- with the probabilities in mind, but it isn't in this version.
-- 
-- Compile with ghc --make resub-fasta.hs -o resub-fasta.ghc_run
-- Run with "./rsub-fasta.ghc_run %A" where %A is the parameter


Base : Type
Base = Bits8

--type Base = Word8
data BaseFrequencyTree = Node Base
                       | TreeNodes Int Base Base
                       | Tree Int BaseFrequencyTree BaseFrequencyTree
data Seed = Seed Int

b2c : Base -> Char
b2c = chr . fromEnum

c2b : Char -> Base
c2b = toEnum . ord

alu : String
alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" ++
      "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" ++
      "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" ++
      "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" ++
      "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" ++
      "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" ++
      "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

im = 139968 :: Double

iub = mkTree $ snd . mapAccumL (\rt (c,f) -> (f+rt,(c2b c,ceiling $ im*(f+rt)))) 0.0 $
  [ ('a', 0.27), ('c', 0.12), ('g', 0.12), ('t', 0.27), ('B', 0.02),
    ('D', 0.02), ('H', 0.02), ('K', 0.02), ('M', 0.02), ('N', 0.02),
    ('R', 0.02), ('S', 0.02), ('V', 0.02), ('W', 0.02), ('Y', 0.02) ]

homosapiens =  mkTree $ snd . mapAccumL (\rt (c,f) -> (f+rt,(c2b c,ceiling $ im*(f+rt)))) 0.0 $
  [ ('a', 0.3029549426680), ('c', 0.1979883004921), ('g', 0.1975473066391), ('t', 0.3015094502008) ]

mkTree [(b,_)] = Node b
mkTree [(b,f),(b',_)] = TreeNodes f b b'
mkTree xs = let (h,t) = splitAt (length xs `div` 2) xs
                (_,f) = last h
            in Tree f (mkTree h) (mkTree t)


-- TODONOTE pattern matching looks different here, | asdf needs a with clause or case statement
chooseBase : BaseFrequencyTree -> Base
chooseBase (Node b) _ = b
chooseBase (TreeNodes f b b') p = if (p<f) then b else b'
chooseBase (Tree f l r) p = case p < f of
                                 True  => chooseBase l p
                                 False => chooseBase r p


writeFastaHeader : String -> String -> IO ()
writeFastaHeader label title =  (putStrLn (('>':: label) ++ (' '::title)))

perLine : Nat
perLine = 60

writeAluBuffer : Nat -> ?fpp
writeAluBuffer totalN = do
  let l = length alu
      bufSize = l + perLine - 1
  aluBuf <- mallocArray bufSize
  foldM_ (\ptr c -> poke ptr (c2b c) >> return (advancePtr ptr 1)) aluBuf (take bufSize (cycle alu))
  let (full,end) = totalN `divMod` perLine
      fullLine n = let ptr = advancePtr aluBuf ((n * perLine) `mod` l)
                   in hPutBuf stdout ptr perLine >> hPutChar stdout '\n'
      lastLine = let ptr = advancePtr aluBuf ((full*perLine) `mod` l)
                 in hPutBuf stdout ptr end >> hPutChar stdout '\n'
  mapM_ fullLine [0..pred full]
  when (end>0) lastLine

writeWrapped : Nat -> Nat -> Nat -> ?fasdfasdf
writeWrapped totalN trans initSeed = do
  seedRef <- newIORef initSeed
  let l = succ perLine
      (im,ia,ic)=(139968,3877,29573)
      nextSeed (Seed s) = Seed ( (s * ia + ic) `mod` im )
      prng = do newSeed <- return.nextSeed =<< readIORef seedRef
                writeIORef seedRef newSeed
                return newSeed
  buf <- mallocArray l
  poke (advancePtr buf perLine) (c2b '\n')
  let (full,end) = totalN `divMod` perLine
      fill 0 _   = return ()
      fill i ptr = do (Seed b) <- prng
                      poke ptr (trans b)
                      fill (pred i) (advancePtr ptr 1)
      fullLine = do fill perLine buf
                    hPutBuf stdout buf l
      lastLine = do fill end buf
                    poke (advancePtr buf end) (c2b '\n')
                    hPutBuf stdout buf (succ end)
  replicateM_ full fullLine
  when (end>0) lastLine
  readIORef seedRef

main = do args <- getArgs
          let n = if null args then 2500000 else read (head args)
          writeFastaHeader "ONE" "Homo sapiens alu"
          writeAluBuffer (2*n)
          writeFastaHeader "TWO" "IUB ambiguity codes"
          seed' <- writeWrapped (3*n) (chooseBase iub) (Seed 42)
          writeFastaHeader "THREE" "Homo sapiens frequency"
          writeWrapped (5*n) (chooseBase homosapiens) seed'
