-- Indexes implemented using mmaped Vectors.
module Index where

import Data.Vector.Storable(Vector)
import qualified Data.Vector.Storable as Vector
import Foreign.Storable
import qualified Data.List as List
import Data.Ord
import Vector

data Index k a =
  Index {
    key :: a -> k,
    get :: Vector a }

{-# INLINEABLE (!) #-}
(!) :: (Ord k1, Storable a) => Index (k1, k2) a -> k1 -> Index k2 a
Index key vec ! k =
  Index (snd . key) $
    findSorted NoGuess key1 k vec
  where
    key1 = fst . key

{-# INLINEABLE filterGT #-}
filterGT :: (Ord k1, Storable a) => k1 -> Index (k1, k2) a -> Index (k1, k2) a
filterGT k (Index key vec) =
  Index key $
    dropWhileSorted NoGuess (\x -> key1 x <= k) $
    vec
  where
    key1 = fst . key

{-# INLINEABLE toList #-}
toList :: (Ord k1, Storable a) => Index (k1, k2) a -> [(k1, Index k2 a)]
toList (Index key vec) =
  case uncons vec of
    Nothing -> []
    Just (x, _) ->
      let
        k = key1 x
        (vec1, vec2) = spanSorted NearStart (\y -> key1 y <= k) vec
      in
        (k, Index (snd . key) vec1):toList (Index key vec2)
  where
    key1 = fst . key

{-# INLINEABLE keys #-}
keys :: (Ord k1, Storable a) => Index (k1, k2) a -> [k1]
keys = map fst . toList

{-# INLINEABLE elems #-}
elems :: (Ord k1, Storable a) => Index (k1, k2) a -> [Index k2 a]
elems = map snd . toList

{-# INLINEABLE intersection #-}
intersection :: (Storable a, Ord k) => [Index k a] -> Index k a
intersection idxs =
  foldl1 intersect (List.sortBy (comparing (Vector.length . get)) idxs)

{-# INLINEABLE intersect #-}
intersect :: (Storable a, Ord k) => Index k a -> Index k a -> Index k a
intersect (Index key vec1) (Index _ vec2) =
  Index key (Vector.fromList (mergeChunks key vec1 vec2))
  where
    mergeChunks key vec1 vec2 =
      case (uncons vec1, uncons vec2) of
        (Nothing, _) -> []
        (_, Nothing) -> []
        (Just (x, rest1), Just (y, rest2)) ->
          let
            fx = key x
            fy = key y in
          case compare fx fy of
            EQ -> x:mergeChunks key rest1 rest2
            LT ->
              let vec1' = dropWhileSorted NearStart (\x' -> key x' < fy) vec1 in
              mergeChunks key vec1' vec2
            GT ->
              let vec2' = dropWhileSorted NearStart (\y' -> key y' < fx) vec2 in
              mergeChunks key vec1 vec2'
