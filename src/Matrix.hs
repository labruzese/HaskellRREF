module Matrix
  ( Matrix (..),
    randomMatrix,
  )
where

import Control.Monad (replicateM)
import System.Random
import Text.Printf (printf)
import Data.List (transpose, intercalate)

data Matrix = Matrix
  { rowCount :: Int,
    colCount :: Int,
    elements :: [[Double]]
  }

instance Show Matrix where
  show (Matrix _ _ rows) = 
    let -- Convert numbers to strings with consistent formatting
        stringRows = map (map (printf "%.2f")) rows
        -- Find the maximum width needed for each column
        colWidths = map (maximum . map length) $ transpose stringRows
        -- Pad each element to match the column width
        padElement width str = replicate (width - length str) ' ' ++ str
        paddedRows = map (zipWith padElement colWidths) stringRows
        -- Join elements and rows with proper spacing and add matrix bars
        formattedRows = map (\row -> "| " ++ intercalate " " row ++ " |") paddedRows
    in unlines formattedRows

randomMatrix :: Int -> Int -> IO Matrix
randomMatrix m n
  | m <= 0 || n <= 0 = error "Matrix dimensions must be positive"
  | otherwise = do
      let generateRow = replicateM n (randomRIO (-100, 100))
      elems <- replicateM m generateRow
      return
        Matrix
          { rowCount = m,
            colCount = n,
            elements = elems
          }
