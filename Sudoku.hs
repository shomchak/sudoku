{-# OPTIONS_GHC -Wall #-}

module Sudoku where

import Text.Read (readMaybe)
import Control.Monad (guard)
import Data.List (nub)
import Data.List.Split (chunksOf)
--import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Class

main :: IO ()
main = getGrid >>= putStrLn . show

-- ----------------------------------------------------------------------------
-- Getting and constructing grids.

-- | A newtype describing a single digit
type Digit = Int

-- | A newtype describing a Sudoku grid of digits
newtype Grid = Grid [Digit] deriving (Eq, Show, Read)

-- | A row of a grid.
type Row = [Digit]

-- | A column of a grid.
type Column = [Digit]

-- | A block of a grid.
type Block = [Digit]

-- | Read a grid from stdin.
getGrid :: IO (Maybe Grid)
--getGrid = getLine >>= return . fromString
getGrid = fmap fromString getLine

-- | Create a grid from a string of digits.
fromString :: String -> Maybe Grid
fromString s = mapM (\c -> readMaybe [c]) s >>= fromList

-- | Create a grid from a list of integers.
fromList :: [Int] -> Maybe Grid
fromList xs = do ds <- mapM fromInt xs
                 guard (length ds == 81)
                 guard (all noDupes $ asRows ds)
                 guard (all noDupes $ asColumns ds)
                 guard (all noDupes $ asBlocks ds)
                 return $ Grid ds

-- | Create a digit from an integer.
fromInt :: Int -> Maybe Digit
fromInt x | 0 <= x && x <= 9 = Just $ x
          | otherwise        = Nothing

-- ----------------------------------------------------------------------------
-- Logic on grids.

-- | Check the list has no duplicate non-zero values.
noDupes :: [Digit] -> Bool
noDupes ds = length nonzeroes == (length $ nub nonzeroes)
                where nonzeroes = filter (/=0) ds

-- | Split a list of digits into rows of 9 elements.
asRows :: [Digit] -> [Row]
asRows = chunksOf 9

-- | Split a list of digits into columns of 9 elements.
asColumns :: [Digit] -> [Column]
asColumns ds = map (\i -> concatMap (return . (!! i)) $ asRows ds) [0..8]

-- | Split a grid into cells of 9 elements.
asBlocks :: [Digit] -> [Block]
asBlocks ds = map (\i -> concat $ take 3 $ takeEvery 3 $ drop i threes)
                  [0,1,2,9,10,11,18,19,20]
                     where threes = concatMap (\row -> chunksOf 3 row) $
                                    asRows ds

slice :: Int -> Int -> [b] -> [b]
slice from to xs = take (to - from + 1) (drop from xs)

takeEvery :: Int -> [b] -> [b]
takeEvery n (x:xs) = x:takeEvery n (drop (n-1) xs)
takeEvery _ []      = []