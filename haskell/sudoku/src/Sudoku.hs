module Sudoku
(makeBoard
,setCell
,solve
 ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type Cells = Map.Map Position Int
type Layout = (Int, Int) -- (numRowsPerArea, numColumnsPerArea)
data Board = MkBoard Layout Cells deriving (Show)

getSize :: Board -> Int
getSize (MkBoard (nRowsArea, nColsArea) _) = nRowsArea * nColsArea

getRow :: Board -> Int -> [Position]
getRow b r = [(r, c) | c <- [0..(size-1)]]
  where
    size = getSize b

getColumn :: Board -> Int -> [Position]
getColumn b c = [(r, c) | r <- [0..(size-1)]]
  where
    size = getSize b

getArea :: Board -> Int -> Int -> [Position]
getArea (MkBoard (nRowsArea, nColsArea) _) x y =
  [(r, c) | r <- [rMin..rMax], c <- [cMin..cMax]]
  where
    rMin = y * nRowsArea
    rMax = rMin + nRowsArea - 1
    cMin = x * nColsArea
    cMax = cMin + nColsArea - 1

getSiblings :: Board -> Position -> [Position]
getSiblings b (r, c) =
  Set.toList (Set.filter (\pos -> pos /= (r, c)) positions)
  where
    MkBoard (nRowsArea, nColsArea) _ = b
    positions = Set.unions [
      Set.fromList rowPositions
      ,Set.fromList colPositions
      ,Set.fromList areaPositions]
    rowPositions = getRow b r
    colPositions = getColumn b c
    areaPositions = getArea b x y
    x = c `div` nColsArea
    y = r `div` nRowsArea

makeBoard :: Int -> Int -> Board
makeBoard nRowsArea nColsArea = MkBoard (nRowsArea, nColsArea) Map.empty

setCell :: Board -> Position -> Int -> Board
setCell (MkBoard layout cells) pos value =
  MkBoard layout (Map.insert pos value cells)

solve :: Board -> Maybe Board
solve b = Nothing -- TODO: implement

getUsedValues :: Board -> [Position] -> Set.Set Int
getUsedValues (MkBoard _ cells) ps =
  Set.fromList $ filter (/= 0) [getValue p | p <- ps]
  where
    getValue p = case (Map.lookup p cells) of
      Just value -> value
      Nothing -> 0

getCandidates :: Board -> Position -> [Int]
getCandidates b pos =
  Set.toList (Set.difference allValues usedValues)
  where
    allValues = Set.fromList [1..size]
    size = getSize b
    usedValues = getUsedValues b $ getSiblings b pos
