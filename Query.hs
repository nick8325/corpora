{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import qualified Data.IntSet as IntSet
import System.TimeIt
import Corpus
import qualified Data.Vector.Storable as Vector
import Index
import Strings
import Data.Reflection
import Data.List
import Data.Ord
import Text.Regex.TDFA
import qualified Data.ByteString.UTF8 as ByteString
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Control.Monad

uniq :: [Int] -> [Int]
uniq xs = go xs IntSet.empty
  where
    go [] _ = []
    go (x:xs) s
      | x `IntSet.member` s = go xs s
      | otherwise = x:go xs (IntSet.insert x s)

query :: WithCorpus => Lemma -> Lemma -> POS -> [[Sentence]]
query w1 w2 w3 =
  map (map getSentence . uniq) $ do
    -- w1 .... w2 w3
    let w1_occ = lemmaIndex ! w1
    (i, w1_is) <- toList w1_occ
    let w2_occ = filterGT i (lemmaIndex ! w2)
    (j, w2_js) <- toList w2_occ
    let w1w2_isjs = intersection [w1_is, w2_js]
    guard (size w1w2_isjs > 0)
    let w3_occ = posIndex ! w3 ! (j+1)
    -- (k, w3_ks) <- toList w3_occ
    return $ map token_sentence (Vector.toList (get (intersection [w1w2_isjs, w3_occ])))

query2 :: WithCorpus => [Lemma] -> [Sentence]
query2 ws =
  map getSentence $ IntSet.toList $ IntSet.fromList $ do
    is <- aux 0 ws
    map token_sentence (Vector.toList (get (intersection is)))
  where
    aux _ [] = return []
    aux i (w:ws) = do
      let occ = filterGT i (lemmaIndex ! w)
      (j, js) <- toList occ
      kss <- aux j ws
      return (js:kss)

next :: WithCorpus => Lemma -> Map Lemma Int
next l1 =
  Map.fromListWith (+)
    [ (l2, 1)
    | tok <- contents (lemmaIndex ! l1),
      tok2 <- contents (sentenceIndex ! token_sentence tok ! (token_position tok + 1)),
      Just l2 <- [lexeme_maybe_lemma (token_lexeme tok2)] ]

top :: Int -> [(a, Int)] -> [(a, Int)]
top n = take n . reverse . sortBy (comparing snd)

main = do
  corpus <- loadCorpus
  withCorpus corpus $ do
--    print (length (Index.toList sentenceIndex))
--    print (length (Index.toList lemmaIndex))
--    print (length (Index.toList posIndex))
--      let l = "in"
--          n = size (lemmaIndex ! l)
--    forM_ (top 20 (counts lemmaIndex)) $ \(l, n) -> do
--      let counts = Map.toList (next l)
--      putStrLn (show l ++ " (" ++ show n ++ "): " ++ show (top 20 counts))
    --print (length (query "cat" "the" "SUBST"))
    --print (length (concat (query "cat" "the" "SUBST")))
    mapM_ (putStrLn . showSentence) (concat (query "cat" "the" "SUBST"))
    --mapM_ (putStrLn . showSentence . getSentence) [ k | (k, x) <- Index.toList sentenceIndex, length (Index.elems x) >= 800]
    --print (length [ k | (k, x) <- Index.counts lemmaIndex, x <= 100])
