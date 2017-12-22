{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import qualified Data.IntSet as IntSet
import System.TimeIt
import Corpus
import qualified Data.Vector.Storable as Vector
import Index
import Data.Reflection

query :: WithCorpus => Lemma -> Lemma -> POS -> [Sentence]
query w1 w2 w3 =
  map getSentence $
  IntSet.toList $ IntSet.fromList $ do
    -- w1 .... w2 w3
    let w1_occ = lemmaIndex ! w1
    (i, w1_is) <- toList w1_occ
    let w2_occ = filterGT i (lemmaIndex ! w2)
    (j, w2_js) <- toList w2_occ
    let w3_occ = posIndex ! w3 ! (j+1)
    -- (k, w3_ks) <- toList w3_occ
    map token_sentence (Vector.toList (get (intersection [w1_is, w2_js, w3_occ])))

main = do
  corpus <- loadCorpus
  withCorpus corpus $
    timeIt (mapM_ (putStrLn . showSentence) (query "cat" "the" "SUBST"))
