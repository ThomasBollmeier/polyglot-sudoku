module Sudoku.IO
(readSudoku)
where

import qualified Sudoku.Core as Core

readSudoku :: String -> IO Core.Board
readSudoku filePath = do
  content <- readFile filePath
  return $ parseSudoku content

parseSudoku :: String -> Core.Board
parseSudoku content = board
  where
    board =Core.setCells (Core.makeBoard nRowsArea nColsArea) pvs
    (nRowsArea, nColsArea, pvs) = parseContent content

parseContent :: String -> (Int, Int, [(Core.Position, Int)])
parseContent content = (nRowsArea, nColsArea, pvs)
  where
    (header:rows) = lines content
    (nRowsArea:nColsArea:[]) = map (\s -> read s::Int) $ words header
    pvs = parseRows rows (nRowsArea * nColsArea)

parseRows :: [String] -> Int -> [(Core.Position, Int)]
parseRows rows size = foldl
  (\acc (rowIdx, row) -> acc ++ parseRow rowIdx row)
  []
  (zip [0..(size-1)] rows)

parseRow :: Int -> String -> [(Core.Position, Int)]
parseRow rowIdx row = [
  ((rowIdx, colIdx), val) |
  (colIdx, valChar) <- zip [0..] row,
  let val = read [valChar] :: Int,
  val > 0]
