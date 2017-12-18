-- Sorting large vectors with the help of temporary files.
module Sort where

import Data.Vector.Storable(Vector)
import Data.Vector.Storable.Mutable(IOVector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Mutable
import qualified Data.Vector.Algorithms.Tim as Tim
import Data.Vector.Storable.MMap
import System.IO.Temp
import Foreign.Storable
import Control.Monad

import Test.QuickCheck
import qualified Data.List as List

slice :: Int -> [(Int, Int)]
slice n = from 0
  where
    k = 1000000
    from m
      | m == n = []
      | otherwise = (m, m' - m):from m'
      where
        m' = (m+k) `min` n

sort :: (Ord a, Storable a) => Vector a -> IO (Vector a)
sort = sortBy compare

sortBy :: Storable a => (a -> a -> Ordering) -> Vector a -> IO (Vector a)
sortBy comp vec =
  withSystemTempFile "sort1" $ \file1 _ ->
  withSystemTempFile "sort2" $ \file2 _ -> do
    let
      n = Vector.length vec
      slices = slice n

    -- Create two temporary vectors to ping-pong between
    input  <- unsafeMMapMVector file1 ReadWriteEx (Just (0, n))
    output <- unsafeMMapMVector file2 ReadWriteEx (Just (0, n))

    -- Copy the input to a temporary vector and sort the slices in-place
    Vector.copy input vec
    forM_ slices $ \(i, n) ->
      Tim.sortBy comp (Mutable.slice i n input)

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

    passes input output slices

merge :: Storable a => (a -> a -> Ordering) -> Vector a -> Vector a -> IOVector a -> IO ()
merge comp in1 in2 out =
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
        
uncons :: Storable a => Vector a -> Maybe (a, Vector a)
uncons vec
  | Vector.null vec = Nothing
  | otherwise = Just (Vector.unsafeHead vec, Vector.unsafeTail vec)

-- Before QuickChecking, reduce "k" in the slice function
prop_sort :: [Int] -> Property
prop_sort xs = ioProperty $ do
  ys <- Vector.toList <$> sort (Vector.fromList xs)
  return $ ys === List.sort xs
