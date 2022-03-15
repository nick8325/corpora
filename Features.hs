{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, FlexibleContexts, UndecidableInstances, RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
import Corpus
import Index
import Strings
import Utils
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Hashable
import Data.Reflection
import GHC.Generics(Generic)
import Data.Binary
import qualified Data.ByteString.Lazy as B
import Debug.Trace
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.List
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString as ByteString
import Control.Monad
import Data.Vector.Storable.ByteString
import Foreign.Storable
import Foreign.Storable.Tuple
import Data.Int
import Foreign.Ptr
import Vector
import Data.Ord
import Database.SQLite.Simple

data WordFeature =
    Text {-# UNPACK #-} !Text
  | Lemma {-# UNPACK #-} !Lemma
  | POS {-# UNPACK #-} !POS
  deriving (Eq, Ord, Hashable, Generic, Binary)

deriving instance Given (StrDatabase BNC) => Show WordFeature

data Feature =
    WordFeature !WordFeature
  | Plus !WordFeature {-# UNPACK #-} !Int !WordFeature
  deriving (Eq, Ord, Hashable, Generic, Binary)

deriving instance Given (StrDatabase BNC) => Show Feature

fromWordFeature :: WordFeature -> (Str BNC, Int8)
fromWordFeature (Text x) = (x, 0)
fromWordFeature (Lemma x) = (x, 1)
fromWordFeature (POS x) = (x, 2)

toWordFeature :: (Str BNC, Int8) -> WordFeature
toWordFeature (x, 0) = (Text x)
toWordFeature (x, 1) = (Lemma x)
toWordFeature (x, 2) = (POS x)

fromFeature :: Feature -> (WordFeature, WordFeature, Int8)
fromFeature (WordFeature f) = (f, f, 0)
fromFeature (Plus w1 n w2) = (w1, w2, fromIntegral n)

toFeature :: (WordFeature, WordFeature, Int8) -> Feature
toFeature (w1, w2, n)
  | n == 0 || n == -1 = WordFeature w1
  | otherwise = Plus w1 (fromIntegral n) w2

instance Storable WordFeature where
  sizeOf = sizeOf . fromWordFeature
  alignment = alignment . fromWordFeature
  peek ptr = toWordFeature <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromWordFeature x)

instance Storable Feature where
  sizeOf = sizeOf . fromFeature
  alignment = alignment . fromFeature
  peek ptr = toFeature <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromFeature x)

features :: Given Corpus => Sentence -> [Feature]
features = usort . loop . map wordFeatures
  where
    loop [] = []
    loop (fs:fss) =
      map WordFeature fs ++
      [ Plus f n f'
      | (n, fs') <- zip [1..] (take bound fss),
        f <- fs,
        f' <- fs' ] ++
      loop fss
    wordFeatures Word{..} =
      [Lemma lexeme_lemma | size (lemmaIndex ! lexeme_lemma) >= 1000] ++
      [POS lexeme_pos]
    wordFeatures _ = []
    bound = 3

countFeatures :: Given Corpus => [Sentence] -> Map Feature Int
countFeatures xs = Map.fromListWith (+) (zip (concatMap features xs) (repeat 1))

indexFeatures :: (Given Corpus, Given (StrDatabase BNC)) => (Feature -> Bool) -> [SentenceNumber] -> Map Feature IntSet
indexFeatures p sentences =
  Map.fromListWith IntSet.union (concatMap index1 sentences)
  where
    index1 s = zip (filter p (features (getSentence s))) (repeat (IntSet.singleton s))

main = do
  corpus <- loadCorpus
  withCorpus corpus $ do
    fs <- readData "data/all-features-unsorted" :: IO (Vector.Vector (Feature, (SentenceNumber, ())))
    db <- open "features.db"
--    execute_ db "create table features(id integer primary key, word1 integer not null, kind1 integer not null, distance integer not null, word2 integer not null, kind2 integer not null, unique(word1, kind1, distance, word2, kind2))"
--    withTransaction db $ do
--      executeMany db "insert or ignore into features(word1, kind1, distance, word2, kind2) values(?, ?, ?, ?, ?)"
--        [ (strId word1, kind1, distance, strId word2, kind2)
--        | (f, (s, ())) <- Vector.toList fs,
--          let (wf1, wf2, distance) = fromFeature f,
--          let (word1, kind1) = fromWordFeature wf1,
--          let (word2, kind2) = fromWordFeature wf2 ]
    --execute_ db "create table sentencefeatures(feature integer not null references features(id), sentence integer not null, primary key(feature, sentence))"
    features <- fold_ db "select id, word1, kind1, distance, word2, kind2 from features" Map.empty
      (\m (id :: Int, w1, k1, d, w2, k2) -> return $! (Map.insert (toFeature (toWordFeature (Str w1, k1), toWordFeature (Str w2, k2), d)) id m))
    putStrLn "read features"
    withTransaction db $ do
      executeMany db "insert or ignore into sentencefeatures(feature, sentence) values(?, ?)"
        [ (Map.findWithDefault undefined f features, s)
        | (f, (s, ())) <- Vector.toList fs ]

--    fs <- readData "data/all-features" :: IO (Vector.Vector (Feature, (SentenceNumber, ())))
--    let idx = Index id fs
--    let query = [WordFeature (POS "ADJ"),
--                 WordFeature (Lemma "time"),
--                 Plus (POS "ADJ") 1 (POS "ADJ"),
--                 Plus (POS "ADJ") 1 (Lemma "time"),
--                 Plus (POS "ADJ") 2 (Lemma "time")]
--    print [(f, size (idx ! f)) | f <- q]
--    let result = intersection [idx ! f | f <- query]
--    mapM_ (putStrLn . showSentence . getSentence) (keys result)

--    fs <- readData "data/all-features-unsorted" :: IO (Vector.Vector (Feature, (SentenceNumber, ())))
--    Vector.sort fs >>= writeData "data/all-features"
--    writeFile "data/all-features-unsorted" ""
--    forM_ (chunk 100000 [(f, (s, ())) | s <- keys sentenceIndex, f <- features (getSentence s)]) $ \fs ->
--      ByteString.appendFile "data/all-features-unsorted" (vectorToByteString (Vector.fromList fs))
--    fsl <- Vector.toList <$> readData "feature-counts" :: IO [(Feature, Int)]
--    let featureCounts = Map.fromAscList fsl
--
--    let lengths = reverse (Data.List.sort (Map.elems featureCounts))
--    print (length (takeWhile (>= 10) lengths))
--    print (sum (takeWhile (>= 10) (dropWhile (>= 10000) lengths)))
--    print (sum lengths)

    --let lengths = sort (map (length . features . getSentence) (keys sentenceIndex))
    --print (lengths !! (length lengths - 1000))
    --putStrLn (showSentence (getSentence 1000000))
    --print (length (features (getSentence 1000000)))
    --mapM_ print (features (getSentence 1000))
    --featureCounts <- decode <$> B.readFile "features" :: IO (Map Feature Int)
--    let ok f = featureCounts Map.! f >= 50
--    B.writeFile "sentence-features" $ encode $ indexFeatures ok (map traceShowId (keys sentenceIndex))
