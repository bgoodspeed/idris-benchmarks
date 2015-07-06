--
-- The Computer Language Benchmarks Game
--   http://shootout.alioth.debian.org/
--
--   Sterling Clover's translation of Tim Hochberg's Clean implementation

module Main

import Control.Arrow

--- The Board ---
n_elem = 5
n_col = 5
n_row = 10

m_top : Mask
m_top = 0x1F

cells : [Cell]
cells = [0..49]

colors : [Color]
colors = [0..9]

infixr 4 .&. 
(.&.) : Bits -> Bits -> Bits

infixr 3 .|.
(.|.) : Bits -> Bits -> Bits

cellAt x y = x + n_col * y
coordOf i = snd &&& fst $ i `quotRem` n_col
isValid x y = 0 <= x && x < n_col && 0 <= y && y < n_row

--- Piece Operations ---
data Direction = E | SE | SW | W | NW | NE deriving (Enum, Eq, Ord)
type Piece = [Direction]
type CellCoord = (Int, Int)
type Mask = Int; type Color = Int; type Row = Int;
type Col = Int; type Tag = Int; type Cell = Int
type Solution = [Mask]

pieces : Array Int Piece
pieces = array (0,9) $ zip [0..9] $
         [[E,  E,  E,  SE],
	  [SE, SW, W,  SW],
	  [W,  W,  SW, SE],
	  [E,  E,  SW, SE],
	  [NW, W,  NW, SE, SW],
	  [E,  E,  NE, W],
	  [NW, NE, NE, W],
	  [NE, SE, E,  NE],
	  [SE, SE, E,  SE],
	  [E,  NW, NW, NW]]

permutations : Piece -> [Piece]
permutations p = take 12 (perms p)
    where
      perms p = p::(flip p) :: perms (rotate p)
      rotate piece = map r piece
          where r E  = NE
                r NE = NW
                r NW = W
                r W  = SW
                r SW = SE
                r SE = E
      flip piece = map f piece
          where f E  = W
                f NE = NW
                f NW = NE
                f W  = E
                f SW = SE
                f SE = SW

--- Mask Operations ----
untag : Mask -> Mask
untag mask   = mask .&. 0x1ffffff

retag : Mask -> Tag -> Mask
retag mask n = untag mask .|. n `shiftL` 25

tagof : Mask -> Tag
tagof mask   = mask `shiftR` 25

tag : Mask -> Tag -> Mask
tag   mask n = mask .|. n `shiftL` 25

count1s : Mask -> Int
count1s i = case i == 0 of
                 True  => 0
                 False => case i .&. 1 == 1 of
                               True  => 1 + count1s(i `shiftR` 1)
                               False => count1s (i `shiftR` 1)



first0 : Mask -> Int
first0 i = case i .&. 1 == 0 of
                True  => 0
                False => 1 + first0 (i `shiftR` 1)

--- Making the Bitmasks ---
mod2 x = x .&. 1
packSize a b = a*5+b
unpackSize n = quotRem n 5

move : Direction -> CellCoord -> CellCoord
move E  (x, y) = (x+1, y)
move W  (x, y) = (x-1, y)
move NE (x, y) = (x+(mod2 y),   y-1)
move NW (x, y) = (x+(mod2 y)-1, y-1)
move SE (x, y) = (x+(mod2 y),   y+1)
move SW (x, y) = (x+(mod2 y)-1, y+1)

y0ValueFor : Bool -> Nat
y0ValueFor True = 1
y0ValueFor False = 0

bndsInner : Piece -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
bnds [] _ _ xmin ymin xmax ymax = (xmin, ymin, xmax, ymax)
bnds (d::rest) x y xmin ymin xmax ymax = let (x', y') = move d (x, y) in
                                          bnds rest x' y' (min x' xmin) (min y' ymin) (max x' xmax) (max y' ymax)


pieceBounds : Piece -> Bool -> (Int, Int, Int, Int)
pieceBounds piece isodd = let y0 = y0ValueFor isodd in
                          bndsInner piece 0 y0 0 y0 0 y0


msk : Piece -> Col -> Row -> Mask -> Mask
msk [] x y m        = m `setBit` cellAt x y
msk (d::rest) x y m = let (x', y') = move d (x, y) in
                          msk rest x' y' (m `setBit` cellAt x y)



pieceMask : Piece -> (Mask, Mask)
pieceMask piece = let 
    (xmin, ymin, xmax, ymax) = pieceBounds piece False
    (x1, y1) = (-xmin, -ymin)
    w1 = xmax - xmin
    h1 = ymax - ymin
    (xmin', ymin', xmax', ymax') = pieceBounds piece True
    (x2, y2) = (-xmin', (-ymin')+1)
    w2 = xmax' - xmin'
    h2 = ymax' - ymin' in
       case odd y1 of
            True  =>  (tag (msk piece x2 y2 0) (packSize w2 h2),
                       tag (msk piece x1 (y1+1) 0 `shiftR` n_col) (packSize w1 h1))
            False =>  (tag (msk piece x1 y1 0) (packSize w1 h1),
                       tag (msk piece x2 (y2+1) 0 `shiftR` n_col) (packSize w2 h2))
      

templatesForColor : Color -> ([Mask], [Mask])
templatesForColor c = let ps = permutations $ pieces ! c 
                          perms = if c == 5 then take 6 ps else ps in
                          (unzip . map pieceMask) perms

 

--- Looking for Islands ---
noLineIslands : Mask -> Cell -> Cell -> Int -> Bool
noLineIslands mask start stop step = 
    if (fnd testBit . fnd ((not .) . testBit) . fnd testBit)  start > stop then True else False
  where
    fnd : (Mask -> Int -> Bool) -> Int -> Bool
    fnd test x = case x >= 25 of
                      True  =>  25
                      False => if test mask x then x else fnd test (x+step)

noLeftIslands : Mask -> Bool
noLeftIslands  mask  = noLineIslands mask 0 20 5
noRightIslands mask  = noLineIslands mask 4 24 5

noIslands : Mask -> Bool
noIslands board = noisles board (count1s board)

noisles : Mask -> Int -> Bool
noisles _ 30 = True
noisles board ones = let board' = fill board (coordOf (first0 board))
                         ones'  = count1s board' in
                         case (ones' - ones) `rem` n_elem /= 0 of
                              True  => False
                              False => noisles board' ones'


fill : Mask -> CellCoord -> Mask
fill m (x, y) = let i = cellAt x y in
                    case (x < 0 || x >= n_nol) || (y < 0 || y >= 6) || (testBit m i) of
                         True  => m
                         False => foldl (\m d -> fill m (move d (x,y))) (setBit m i) [E, NE, NW, W, SW, SE]


--- More Mask Generation ---
masksForColor : Color -> [(Row, Mask)]
masksForColor c = let (evens, odds) = templatesForColor c in
                      concatMap atCells cells  where
    atCell : Nat -> List (Row, Mask)
    atCell n = let (x, y) = coordOf n in
                case even y of
                    True  => [(y, retag (m `shiftL` x) c) | m <- evens, isok m x y]
                    False => [(y, retag (m `shiftL` x) c) | m <- odds,  isok m x y]

isok : Mask -> Row -> Col -> Bool
isok mask x y = let (width, height) = unpackSize (tagof mask)
                    mask' = untag mask `shiftL` x in
                    isValid (x+width) (y+height) &&
                    case (y == 0, y+height==9) of
                      (False, False) => noLeftIslands mask' && noRightIslands mask'
                      (False, True)  => noIslands (mask' `shiftL` (n_col * (y - 4)))
                      (True, _ ) => noIslands mask'

masksAtCell : Array (Row,Col) (Array Color [Mask])
masksAtCell = trps $ map (masksAt cells . masksForColor) colors

masksAt : [Int] -> [(Row,Mask)]-> [[Mask]]
masksAt [] _ = []
masksAt (n::ns) masks = let (t, f) = partition maskTest masks in
                            map snd t :: masksAt ns f where
  maskTest : (Row, Mask) -> Bool
  maskTest (r, m) = let n' = n - (n_col * r) in
                      (n' >= 0) && (n' < 25) && (m `testBit` n')


-- TODONOTE what is this (,) business?  setup GHC so i can ask hugs etc what the type is
-- TODONOTE also investigate Array etc

trps : [[[Mask]]] -> Array (Row, Col) (Array Color [Mask])
trps a = array ((0,0),(9,4)) $ concatMap (uncurry (map . first . (,))) $
          zip [0..9] [copy !! y | y <- [1,0,1,0,1,2,3,4,5,6]]
    where
      copy = [ [(x,copy' (cellAt x y)) | x <- [0..n_col-1]] |
               y <- [1,2,5,6,7,8,9]]
      copy' cell = array (0,9) $ map (\clr -> (clr,a !! clr !! cell)) colors

--- Formatting ---
format : Bool -> String -> String
format _ [] = ""
format isodd chars | isodd = " " ++ str | otherwise = str
        where
          (cur, rest) = splitAt 5 chars
          str =  intersperse ' ' cur ++ " \n" ++ format (not isodd) rest

toString : Solution -> String
toString masks = map color cells
    where
      masksWithRows = withRows 0 0 (reverse masks)
      withRows _ _ [] = []
      withRows board r (m:rest) = (r', m) : withRows board' r' rest
          where delta = first0 board `quot` n_col
                board' = board `shiftR`  (delta * n_col) .|. untag m
                r' = r+delta
      color n = maybe '.' (("0123456789" !!) . tagof . snd)
                (find matches masksWithRows)
          where
            matches (r, m)
              | n' < 0 || n' > 30  = False
              | otherwise  = (untag m) `testBit` n'
              where n' = n - (n_col * r)

--- Generate the solutions ---
firstZero : UArray Int Int
firstZero = array (0,31) $ zip [0..31]
            [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5]

solutions : [String]
solutions = solveCell 0 colors 0 [] []

solveCell : Row -> [Color] -> Mask -> Solution -> [String] -> [String]
solveCell _ [] board soln results = let s = toString soln
                                    in  s:(reverse s):results 
solveCell row !todo !board !soln results
    | top/=m_top = foldr solveMask results
                   [(m, c) | c <- todo, m  <- masks ! c,  board .&. m == 0]
    | otherwise  = solveCell (row+1) todo (board `shiftR` n_col) soln results
    where top = board .&. m_top
          masks = masksAtCell ! (row, (firstZero ! top) )
          solveMask (!m,!c) results =
              solveCell row (delete c todo) (untag m .|. board) (m:soln) results

main = do
    n <- return.read.head =<< getArgs
    let nsolutions = take n solutions
    putStrLn $ (show $ length nsolutions) ++ " solutions found\n"
    putStrLn . format False . minimum $ nsolutions
    putStrLn . format False . maximum $ nsolutions
