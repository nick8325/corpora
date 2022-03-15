module Utils where

import Data.List
import qualified Data.Set as Set

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

uniq :: Ord a => [a] -> [a]
uniq xs = go xs Set.empty
  where
    go [] _ = []
    go (x:xs) s
      | x `Set.member` s = go xs s
      | otherwise = x:go xs (Set.insert x s)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys:chunk n zs
  where
    (ys, zs) = splitAt n xs
