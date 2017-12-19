{-# LANGUAGE BangPatterns #-}
-- Miscellaneous vector functions.
module Vector(
  uncons, sort, sortBy, writeData, readData,
  Guess(..), countWhileMonotone,
  takeWhileMonotone, dropWhileMonotone, spanMonotone) where

import Data.Vector.Storable(Vector)
import Data.Vector.Storable.Mutable(IOVector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Mutable
import Data.Vector.Storable.MMap
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Algorithms.Search
import Control.Monad
import Foreign.Storable
import System.IO.Unsafe
import System.IO.Temp

import Test.QuickCheck
import qualified Data.List as List

{-# INLINE uncons #-}
uncons :: Storable a => Vector a -> Maybe (a, Vector a)
uncons vec
  | Vector.null vec = Nothing
  | otherwise = Just (Vector.unsafeHead vec, Vector.unsafeTail vec)

-- Sort a large vector with the help of a temporary files.
{-# INLINE sort #-}
sort :: (Ord a, Storable a) => Vector a -> IO (Vector a)
sort = sortBy compare

{-# INLINEABLE sortBy #-}
sortBy :: Storable a => (a -> a -> Ordering) -> Vector a -> IO (Vector a)
sortBy comp vec =
  withSystemTempFile "sort1" $ \file1 _ ->
  withSystemTempFile "sort2" $ \file2 _ -> do
    let
      n = Vector.length vec
      ss = slices n

    -- Create two temporary vectors to ping-pong between
    input  <- unsafeMMapMVector file1 ReadWriteEx (Just (0, n))
    output <- unsafeMMapMVector file2 ReadWriteEx (Just (0, n))

    -- Chop the vector into slices and sort the slices
    forM_ ss $ \(i, n) -> do
      Vector.copy (Mutable.slice i n input) $
        Vector.slice i n vec
      Intro.sortBy comp (Mutable.slice i n input)

    -- Merging
    let
      pass input output [] = return []
      pass input output [(i, n)] = do
        Vector.copy (Mutable.slice i n output) (Vector.slice i n input)
        return [(i, n)]
      pass input output ((i1, n1):(i2, n2):xs) = do
        merge comp
          (Vector.slice i1 n1 input)
          (Vector.slice i2 n2 input)
          (Mutable.slice i1 (n1+n2) output)
        fmap ((i1, n1+n2):) (pass input output xs)

      passes minput output [] = Vector.unsafeFreeze minput
      passes minput output [_] = Vector.unsafeFreeze minput
      passes minput output xs = do
        input <- Vector.unsafeFreeze minput
        ys <- pass input output xs
        passes output minput ys

    passes input output ss

slices :: Int -> [(Int, Int)]
slices n = from 0
  where
    k = 1000000
    from m
      | m == n = []
      | otherwise = (m, m' - m):from m'
      where
        m' = (m+k) `min` n

{-# INLINEABLE merge #-}
merge :: Storable a => (a -> a -> Ordering) -> Vector a -> Vector a -> IOVector a -> IO ()
merge comp !in1 !in2 !out =
  case (uncons in1, uncons in2) of
    (Nothing, _) ->
      Vector.copy out in2
    (_, Nothing) ->
      Vector.copy out in1
    (Just (x, rest1), Just (y, rest2)) ->
      case comp x y of
        LT -> do
          Mutable.write out 0 x
          merge comp rest1 in2 (Mutable.tail out)
        _ -> do
          Mutable.write out 0 y
          merge comp in1 rest2 (Mutable.tail out)

data Guess = NearStart | NearEnd | NoGuess

countWhileMonotone :: Storable a => Guess -> (a -> Bool) -> Vector a -> Int
countWhileMonotone guess p vec =
  unsafeDupablePerformIO $ do
    let q = not . p
    mvec <- Vector.unsafeThaw vec
    case guess of
      NearStart -> gallopingSearchLeftP  q mvec
      NearEnd   -> gallopingSearchRightP q mvec
      NoGuess   -> binarySearchP q mvec

takeWhileMonotone :: Storable a => Guess -> (a -> Bool) -> Vector a -> Vector a
takeWhileMonotone guess p vec =
  Vector.take n vec
  where
    n = countWhileMonotone guess p vec

dropWhileMonotone :: Storable a => Guess -> (a -> Bool) -> Vector a -> Vector a
dropWhileMonotone guess p vec =
  Vector.drop n vec
  where
    n = countWhileMonotone guess p vec

spanMonotone :: Storable a => Guess -> (a -> Bool) -> Vector a -> (Vector a, Vector a)
spanMonotone guess p vec =
  Vector.splitAt n vec
  where
    n = countWhileMonotone guess p vec

writeData :: Storable a => FilePath -> Vector a -> IO ()
writeData file vec = do
  writeFile file "" -- truncate to 0, see comment for writeMMapVector
  writeMMapVector file vec

readData :: Storable a => FilePath -> IO (Vector a)
readData file = unsafeMMapVector file Nothing

-- Before QuickChecking, reduce "k" in the slice function
prop_sort :: [Int] -> Property
prop_sort xs = ioProperty $ do
  ys <- Vector.toList <$> sort (Vector.fromList xs)
  return $ ys === List.sort xs
