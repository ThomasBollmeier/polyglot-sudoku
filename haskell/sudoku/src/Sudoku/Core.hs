module Sudoku.Core
(Board
,Position
,makeBoard
,setCell
,setCells
,solve
 ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

type Position = (Int, Int)
type Cells = Map.Map Position Int
type Layout = (Int, Int) -- (numRowsPerArea, numColumnsPerArea)
data Board = MkBoard Layout Cells

instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard b = foldl (\s rowStr -> s ++ rowStr ++ "\n" ) "" rowStrs
  where
    rowStrs = map (showRow b) [0..(size-1)]
    size = getSize b

showRow :: Board -> Int -> String
showRow b r = foldl (\s c -> s ++ showCell b (r, c)) "" [0..(size-1)]
  where
    size = getSize b

showCell :: Board -> Position -> String
showCell (MkBoard _ cells) p =
  case Map.lookup p cells of
    Just value -> show value
    Nothing -> "_"

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
getSiblings b@(MkBoard (nRowsArea, nColsArea) _) (r, c) =
  Set.toList (Set.filter (\pos -> pos /= (r, c)) positions)
  where
    positions = Set.unions [
      Set.fromList rowPositions
      ,Set.fromList colPositions
      ,Set.fromList areaPositions]
    rowPositions = getRow b r
    colPositions = getColumn b c
    areaPositions = getArea b x y
    x = c `div` nColsArea
    y = r `div` nRowsArea

getGroups :: Board -> [[Position]]
getGroups b =
  [getRow b r | r <- [0..maxIdx]] ++
  [getColumn b c | c <- [0..maxIdx]] ++
  [getArea b x y | x <- [0..maxX], y <- [0..maxY]]
  where
    (MkBoard (nRowsArea, nColsArea) _) = b
    maxIdx = nRowsArea * nColsArea - 1
    maxX = nRowsArea - 1
    maxY = nColsArea - 1

makeBoard :: Int -> Int -> Board
makeBoard nRowsArea nColsArea = MkBoard (nRowsArea, nColsArea) Map.empty

setCell :: Board -> Position -> Int -> Board
setCell (MkBoard layout cells) pos value =
  MkBoard layout (Map.insert pos value cells)

setCells :: Board -> [(Position, Int)] -> Board
setCells b pvs = foldl (\b (p,v) -> setCell b p v) b pvs

solve :: Board -> Maybe Board
solve b = if not (isValid b)
  then Nothing
  else case solutions of
    Nothing -> Just b -- no free position => we are done
    Just [] -> Nothing -- no valid alternative => error
    Just (mb:_) -> mb -- solution found
  where
    solutions = do
      p <- getFreePosition b
      let vs = getCandidates b p
      let mbs = map (solve' b p) vs
      return $ dropWhile (not . maybeIsValid) mbs
    maybeIsValid = \mb -> case mb of
      Just board -> isValid board
      Nothing -> False

solve' :: Board -> Position -> Int -> Maybe Board
solve' b p v =
  if isValid b'
    then solve b'
    else Nothing
  where
    b' = setCell b p v

getUsedValues :: Board -> [Position] -> Set.Set Int
getUsedValues (MkBoard _ cells) ps =
  Set.fromList usedValues
    where
      usedValues = catMaybes values
      values = fmap (\p -> Map.lookup p cells) ps

getCandidates :: Board -> Position -> [Int]
getCandidates b pos =
  Set.toList (Set.difference allValues usedValues)
  where
    allValues = Set.fromList [1..size]
    size = getSize b
    usedValues = getUsedValues b $ getSiblings b pos

getFreePosition :: Board -> Maybe Position
getFreePosition b =
  if not (null freePositions)
    then Just (head freePositions)
    else Nothing
  where
    (MkBoard _ cells) = b
    maxIdx = (getSize b) - 1
    allPositions = Set.fromList [(r, c) | r <- [0..maxIdx], c <- [0..maxIdx]]
    fixedPositions = Set.fromList $ Map.keys cells
    freePositions = Set.toList $ Set.difference allPositions fixedPositions

isGroupValid :: Board -> [Position] -> Bool
isGroupValid b ps = length fixedCells == Set.size fixedValues
  where
    (MkBoard _ cells) = b
    fixedCells = filter (\p -> p `Map.member` cells) ps
    fixedValues = getUsedValues b ps

isValid :: Board -> Bool
isValid b = all (isGroupValid b) (getGroups b)
