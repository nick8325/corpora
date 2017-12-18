{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Corpora where

import System.IO.Unsafe
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap(IntMap)
import Data.Binary(Binary, encodeFile, decodeFile)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Vector as Vector
import Data.Csv
import Control.Monad

{-# NOINLINE cacheC5 #-}
{-# NOINLINE cachePos #-}
{-# NOINLINE cacheHW #-}
{-# NOINLINE cacheText #-}
cacheC5, cachePos, cacheHW, cacheText :: IntMap String
cacheC5 = unsafePerformIO (decodeFile "cache.c5")
cachePos = unsafePerformIO (decodeFile "cache.pos")
cacheHW = unsafePerformIO (decodeFile "cache.hw")
cacheText = unsafePerformIO (decodeFile "cache.text")

showCache :: IntMap String -> Int -> String
showCache map x = fromJust (IntMap.lookup x map)

makeCache :: String -> IO ()
makeCache base = do
  file <- ByteString.readFile ("csv." ++ base)
  let
    kvs :: Vector.Vector (Int, String)
    Right kvs = decode NoHeader file
  encodeFile ("cache." ++ base) (IntMap.fromList (Vector.toList kvs))

makeCaches :: IO ()
makeCaches = mapM_ makeCache ["c5", "pos", "hw", "text"]

newtype C5 = C5 Int deriving (Eq, Ord, Binary)
newtype Pos = Pos Int deriving (Eq, Ord, Binary)
newtype HW = HW Int deriving (Eq, Ord, Binary)
newtype Text = Text Int deriving (Eq, Ord, Binary)
instance Show C5 where show (C5 x) = showCache cacheC5 x
instance Show Pos where show (Pos x) = showCache cachePos x
instance Show HW where show (HW x) = showCache cacheHW x
instance Show Text where show (Text x) = showCache cacheText x

data Token =
  Word {
    c5 :: {-# UNPACK #-} !C5,
    pos :: {-# UNPACK #-} !Pos,
    headWord :: {-# UNPACK #-} !HW,
    text :: {-# UNPACK #-} !Text,
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

instance FromRecord Token where
  parseRecord r = do
    (mtext, mc5, mhw, mpos, sentence, word) <-
      parseRecord r :: Parser (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Int, Int)
    case (mc5, mpos, mhw, mtext) of
      (Just c5, Just pos, Just hw, Just text) ->
        return (Word (C5 c5) (Pos pos) (HW hw) (Text text) sentence word)
      (Just c5, Nothing, Nothing, Just text) ->
        return (Char (C5 c5) (Text text) sentence word)
      (Nothing, Nothing, Nothing, Nothing) ->
        return (Gap sentence word)
      _ -> fail ("strange fields available: " ++ show (mc5, mpos, mhw, mtext, sentence, word))
