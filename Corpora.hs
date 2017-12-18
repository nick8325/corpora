{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Corpora where

import System.IO.Unsafe
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap(IntMap)
import Data.Binary(Binary, encodeFile, decodeFile)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as DataVector
import Data.Csv
import Control.Monad
import Data.Int
import Data.Vector.Storable.MMap
import Foreign.Storable
import Foreign.Ptr
import Vector
import Data.Ord
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable.MMap
import System.Posix.Files

data Token =
  Word {
    c5 :: {-# UNPACK #-} !C5,
    text :: {-# UNPACK #-} !Text,
    pos :: {-# UNPACK #-} !Pos,
    headWord :: {-# UNPACK #-} !HW,
    sentence :: {-# UNPACK #-} !Int,
    word :: {-# UNPACK #-} !Int } |
  Char {
    c5 :: {-# UNPACK #-} !C5,
    text :: {-# UNPACK #-} !Text,
    sentence :: {-# UNPACK #-} !Int,
    word :: {-# UNPACK #-} !Int } |
  Gap {
    sentence :: {-# UNPACK #-} !Int,
    word :: {-# UNPACK #-} !Int }
  deriving (Eq, Show)

headWordMaybe :: Token -> Maybe HW
headWordMaybe x@Word{} = Just (headWord x)
headWordMaybe _ = Nothing

instance FromRecord Token where
  parseRecord r = do
    (mtext, mc5, mhw, mpos, sentence, word) <-
      parseRecord r :: Parser (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Int, Int)
    case (mc5, mpos, mhw, mtext) of
      (Just c5, Just pos, Just hw, Just text) ->
        return (Word (C5 c5) (Text text) (Pos pos) (HW hw) sentence word)
      (Just c5, Nothing, Nothing, Just text) ->
        return (Char (C5 c5) (Text text) sentence word)
      (Nothing, Nothing, Nothing, Nothing) ->
        return (Gap sentence word)
      _ -> fail ("strange fields available: " ++ show (mc5, mpos, mhw, mtext, sentence, word))

instance Storable Token where
  sizeOf _ = 6 * sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int64)
  peek ptr = do
    let
      peek32 ptr n = do
        x :: Int32 <- peekElemOff ptr n
        return (fromIntegral x :: Int)

    a <- peek32 (castPtr ptr) 0
    b <- peek32 (castPtr ptr) 1
    c <- peek32 (castPtr ptr) 2
    d <- peek32 (castPtr ptr) 3
    e <- peek32 (castPtr ptr) 4
    f <- peek32 (castPtr ptr) 5
    let
      unpack 0 0 0 0 e f = Gap e f
      unpack a b 0 0 e f = Char (C5 a) (Text b) e f
      unpack a b c d e f = Word (C5 a) (Text b) (Pos c) (HW d) e f

    return (unpack a b c d e f)

  poke ptr x = do
    let
      poke32 ptr n (x :: Int) =
        pokeElemOff ptr n (fromIntegral x :: Int32)

      pack a b c d e f = do
        poke32 (castPtr ptr) 0 a
        poke32 (castPtr ptr) 1 b
        poke32 (castPtr ptr) 2 c
        poke32 (castPtr ptr) 3 d
        poke32 (castPtr ptr) 4 e
        poke32 (castPtr ptr) 5 f

    case x of
      Gap e f -> pack 0 0 0 0 e f
      Char (C5 a) (Text b) e f -> pack a b 0 0 e f
      Word (C5 a) (Text b) (Pos c) (HW d) e f -> pack a b c d e f

newtype C5 = C5 Int deriving (Eq, Ord, Binary)
newtype Pos = Pos Int deriving (Eq, Ord, Binary)
newtype HW = HW Int deriving (Eq, Ord, Binary)
newtype Text = Text Int deriving (Eq, Ord, Binary)
instance Show C5 where show (C5 x) = showData dataC5 x
instance Show Pos where show (Pos x) = showData dataPos x
instance Show HW where show (HW x) = showData dataHW x
instance Show Text where show (Text x) = showData dataText x

{-# NOINLINE wordsVec #-}
wordsVec :: Vector.Vector Token
wordsVec = unsafePerformIO (unsafeMMapVector "data/words" Nothing)

{-# NOINLINE sortedWords #-}
sortedWords :: Vector.Vector Token
sortedWords = unsafePerformIO (unsafeMMapVector "data/words-hw-word-sentence" Nothing)

{-# NOINLINE dataC5 #-}
{-# NOINLINE dataPos #-}
{-# NOINLINE dataHW #-}
{-# NOINLINE dataText #-}
dataC5, dataPos, dataHW, dataText :: IntMap String
dataC5 = unsafePerformIO (decodeFile "data/c5")
dataPos = unsafePerformIO (decodeFile "data/pos")
dataHW = unsafePerformIO (decodeFile "data/hw")
dataText = unsafePerformIO (decodeFile "data/text")

showData :: IntMap String -> Int -> String
showData map x = fromJust (IntMap.lookup x map)

makeDataFile :: String -> IO ()
makeDataFile base = do
  file <- ByteString.readFile ("csv/" ++ base)
  let
    kvs :: DataVector.Vector (Int, String)
    Right kvs = decode NoHeader file
  encodeFile ("data/" ++ base)
    (IntMap.fromList (DataVector.toList kvs))

makeData :: IO ()
makeData = do
  mapM_ makeDataFile ["c5", "pos", "hw", "text"]

makeWords :: String -> IO ()
makeWords base = do
  file <- ByteString.readFile ("csv/" ++ base)
  let
    kvs :: DataVector.Vector Token
    Right kvs = decode NoHeader file

    out = "data/" ++ base

  setFileSize out 0
  writeMMapVector out kvs

makeIndex :: Ord a => String -> (Token -> a) -> IO ()
makeIndex base f = do
  sorted <- sortBy (comparing f) wordsVec
  let out = "data/words-" ++ base
  setFileSize out 0
  writeMMapVector out sorted

main = do
  makeIndex "hw-word-sentence" (\x -> (headWordMaybe x, word x, sentence x))
