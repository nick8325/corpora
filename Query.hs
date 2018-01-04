{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import qualified Data.IntSet as IntSet
import System.TimeIt
import Corpus
import qualified Data.Vector.Storable as Vector
import Index
import Strings
import Data.Reflection
import Text.Regex.TDFA
import qualified Data.ByteString.UTF8 as ByteString

uniq :: [Int] -> [Int]
uniq xs = go xs IntSet.empty
  where
    go [] _ = []
    go (x:xs) s
      | x `IntSet.member` s = go xs s
      | otherwise = x:go xs (IntSet.insert x s)

query :: WithCorpus => Lemma -> Lemma -> POS -> [Sentence]
query w1 w2 w3 =
  map getSentence $ uniq $ do
    -- w1 .... w2 w3
    let w1_occ = lemmaIndex ! w1
    (i, w1_is) <- toList w1_occ
    let w2_occ = filterGT i (lemmaIndex ! w2)
    (j, w2_js) <- toList w2_occ
    let w3_occ = posIndex ! w3 ! (j+1)
    -- (k, w3_ks) <- toList w3_occ
    map token_sentence (Vector.toList (get (intersection [w1_is, w2_js, w3_occ])))

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

main = do
  corpus <- loadCorpus
  withCorpus corpus $ do
    mapM_ (putStrLn . showSentence) (query "cat" "the" "SUBST")
    --mapM_ (putStrLn . showSentence . getSentence) [ k | (k, x) <- Index.toList sentenceIndex, length (Index.elems x) >= 800]
