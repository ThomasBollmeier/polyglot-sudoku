module Main where

import System.Environment
import Data.Maybe
import qualified Sudoku.Core as SC
import qualified Sudoku.IO as SIO

main :: IO ()
main = do
  (filePath:_) <- getArgs
  solveProblemFromFile filePath

solveProblemFromFile :: String -> IO ()
solveProblemFromFile filePath = do
  board <- SIO.fromSudokuFile filePath
  let solution = SC.solve board
  putStrLn (show $ fromJust solution)
