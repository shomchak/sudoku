{-# OPTIONS_GHC -Wall #-}

module Sudoku where

import Text.Read (readMaybe)
import Control.Monad (guard, join)
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (nub)

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
getGrid = fmap fromString getLine

-- | Create a grid from a string of digits.
fromString :: String -> Maybe Grid
fromString s = mapM (\c -> readMaybe [c]) s >>= fromList

-- | Create a grid from a list of integers.
fromList :: [Int] -> Maybe Grid
fromList xs = do ds <- mapM fromInt xs
                 guard $ gridValid ds
                 return $ Grid ds

-- | Create a digit from an integer.
fromInt :: Int -> Maybe Digit
fromInt x | 0 <= x && x <= 9 = Just x
          | otherwise        = Nothing

-- ----------------------------------------------------------------------------
-- Grid validation.

-- | Check the list has no duplicate non-zero values.
noDupes :: [Digit] -> Bool
noDupes ds = length nonzeroes == (length $ nub nonzeroes)
                where nonzeroes = filter (/=0) ds

-- | Split a list of digits into rows of 9 elements.
asRows :: [Digit] -> [Row]
asRows = groups 9

-- | Split a list of digits into columns of 9 elements.
asColumns :: [Digit] -> [Column]
asColumns ds = map (\i -> concatMap (return . (!! i)) $ asRows ds) [0..8]

-- | Split a grid into cells of 9 elements.
asBlocks :: [Digit] -> [Block]
asBlocks ds = map (\i -> concat $ take 3 $ takeEvery 3 $ drop i threes)
                  [0,1,2,9,10,11,18,19,20]
                     where threes = concatMap (\row -> groups 3 row) $
                                    asRows ds

-- | Check a grid has the right size, and that there are no illegal duplicates.
gridValid :: [Digit] -> Bool
gridValid ds = (length ds == 81) &&
               (all noDupes $ join $ [asRows, asColumns, asBlocks] <*> pure ds)

-- ----------------------------------------------------------------------------
-- Grid access.

-- | Return the row of digits to which a certain cell belongs.
rowOf :: Int -> [Digit] -> Maybe Row
rowOf n ds | validIndex n = Just $ asRows ds !! quot n 9
           | otherwise    = Nothing

-- | Return the column of digits to which a certain cell belongs.
columnOf :: Int -> [Digit] -> Maybe Column
columnOf n ds | validIndex n = Just $ asColumns ds !! mod n 9
              | otherwise    = Nothing

-- | Return the cell of digits to which a certain cell belongs
blockOf :: Int -> [Digit] -> Maybe Block
blockOf n ds | validIndex n = Just $ asBlocks ds !! i
             | otherwise    = Nothing
                where x = quot (mod n 9) 3
                      y = quot (quot n 9) 3
                      i = x + 3 * y

-- ----------------------------------------------------------------------------
-- Grid solving.

-- | Return a list of what digits could occupy a cell.
choices :: [Digit] -> Int -> Maybe [Digit]
choices ds n = do taken <- m_taken
                  filter (not . (`elem` taken)) <$> pure [1..9]
                     where takens  = [rowOf, columnOf, blockOf] <*>
                                       pure n <*> pure ds
                           m_taken = sequence takens >>= Just . concat . nub

-- ----------------------------------------------------------------------------
-- Helpers.

validIndex :: Int -> Bool
validIndex n = 0 <= n && n < 81

-- | Group a list into continuous sublists of a certain size.
groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n l  = (take n l):(groups n $ drop n l)

-- | Return every nth term of a list.
takeEvery :: Int -> [b] -> [b]
takeEvery _ []     = []
takeEvery n (x:xs) = x:takeEvery n (drop (n-1) xs)

-- | Safe list indexing.
(!!!) :: [a] -> Int -> Maybe a
l !!! n | 0 <= n && n < length l = Just $ l !! n
        | otherwise              = Nothing
