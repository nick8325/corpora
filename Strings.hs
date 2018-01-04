-- Interned strings, backed by an on-disk database.
{-# LANGUAGE DeriveGeneric, FlexibleContexts, UndecidableInstances, RecordWildCards, BangPatterns, GeneralizedNewtypeDeriving #-}
module Strings(
  Str(..), strId, strValue, strValueBS,
  StrDatabase, newStrDatabase, loadStrDatabase, saveStrDatabase,
  intern, internBS, unintern, uninternBS) where

import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable(Vector, (!))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as ByteString(toString, fromString)
import qualified Data.ByteString.Lazy as ByteString(toStrict, fromStrict)
import Data.ByteString(ByteString)
import GHC.Generics
import Data.String
import Data.Reflection
import Data.IORef
import System.IO.Unsafe
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap(IntMap)
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import Vector
import Data.Binary
import Data.Vector.Binary
import qualified Data.List as List
import Data.Ord
import Foreign.Storable
import Data.Int

-- An interned string is really just a number.
-- The phantom type parameter represents the particular database.
newtype Str s = Str Int32 deriving (Eq, Ord, Storable)

strId :: Str s -> Int
strId (Str n) = fromIntegral n

strValue :: Given (StrDatabase s) => Str s -> String
strValue str = unintern given str

strValueBS :: Given (StrDatabase s) => Str s -> ByteString
strValueBS str = uninternBS given str

showStrNum :: Str s -> String
showStrNum (Str n) = show n
    
instance Given (StrDatabase s) => IsString (Str s) where
  fromString = intern given

instance Given (StrDatabase s) => Show (Str s) where
  show = show . unintern given

newtype StrDatabase s = StrDatabase (IORef Contents)

-- The database has both an on-disk part and an in-memory part.
-- This is so that asking to intern a string which is not stored on disk
-- does not cause an error. Newly interned strings are not automatically
-- saved to disk, but you can ask to write the entire database to disk.
data Contents =
  Contents {
    contents_disk     :: {-# UNPACK #-} !DiskContents,
    contents_memory   :: {-# UNPACK #-} !MemoryContents }
  deriving (Eq, Show)

data MemoryContents =
  MemoryContents {
    -- The next number to use.
    mc_next     :: {-# UNPACK #-} !Int32,
    -- Maps from strings to their numbers and back again.
    mc_intern   :: !(Map ByteString Int32),
    mc_unintern :: !(IntMap ByteString) }
  deriving (Eq, Show)

data DiskContents =
  DiskContents {
    -- All interned strings concatenated together.
    dc_string  :: {-# UNPACK #-} !ByteString,
    -- dc_offsets[i] gives the offset of string number i in dc_string
    dc_offsets :: {-# UNPACK #-} !(Vector Int),
    -- dc_lengths[i] gives the length of string number i
    dc_lengths :: {-# UNPACK #-} !(Vector Int),
    -- dc_sorted[i] gives the offset of the ith smallest string in dc_string
    dc_sorted  :: {-# UNPACK #-} !(Vector Int) }
  deriving (Eq, Show, Generic)

instance Binary DiskContents

----------------------------------------------------------------------
-- Interning and uninterning.
----------------------------------------------------------------------

intern :: StrDatabase s -> String -> Str s
intern db s = internBS db (ByteString.fromString s)

internBS :: StrDatabase s -> ByteString -> Str s
internBS (StrDatabase ref) !str =
  unsafeDupablePerformIO $
  atomicModifyIORef' ref $ \contents ->
    internContents contents str

internContents :: Contents -> ByteString -> (Contents, Str s)
internContents contents@Contents{contents_disk = disk@DiskContents{..}, contents_memory = MemoryContents{..}} str =
  case Vector.toList (findSorted NoGuess (readString disk) str dc_sorted) of
    [n] -> (contents, Str (fromIntegral n))
    [] ->
      case Map.lookup str mc_intern of
        Just n -> (contents, Str n)
        Nothing ->
          let n = mc_next in
          (contents {
             contents_memory =
               MemoryContents {
                 mc_next = n+1,
                 mc_intern = Map.insert str n mc_intern,
                 mc_unintern = IntMap.insert (fromIntegral n) str mc_unintern } },
            Str n)
    _ -> error "duplicate interned strings"

unintern :: StrDatabase s -> Str s -> String
unintern db str = ByteString.toString (uninternBS db str)

uninternBS :: StrDatabase s -> Str s -> ByteString
uninternBS (StrDatabase ref) !str =
  unsafeDupablePerformIO $ do
    contents <- readIORef ref
    return (uninternContents contents str)

uninternContents :: Contents -> Str s -> ByteString
uninternContents Contents{contents_disk = disk@DiskContents{..}, contents_memory = MemoryContents{..}} str
  | strId str < Vector.length dc_offsets =
    readString disk (strId str)
  | otherwise =
    case IntMap.lookup (strId str) mc_unintern of
      Just str -> str
      Nothing -> error "unknown interned string"

-- Read the value of a string from the on-disk part of the database.
readString :: DiskContents -> Int -> ByteString
readString DiskContents{..} n =
  ByteString.take (dc_lengths ! n) $
  ByteString.drop (dc_offsets ! n) dc_string

----------------------------------------------------------------------
-- Creating, loading and storing databases.
----------------------------------------------------------------------

newStrDatabase :: IO (StrDatabase s)
newStrDatabase =
  newStrDatabaseFrom
    DiskContents {
      dc_string = ByteString.empty,
      dc_offsets = Vector.empty,
      dc_lengths = Vector.empty,
      dc_sorted = Vector.empty }

newStrDatabaseFrom :: DiskContents -> IO (StrDatabase s)
newStrDatabaseFrom disk =
  StrDatabase <$>
  newIORef Contents {
    contents_disk = disk,
    contents_memory =
      MemoryContents {
        mc_next = fromIntegral (Vector.length (dc_offsets disk)),
        mc_intern = Map.empty,
        mc_unintern = IntMap.empty } }
        
loadStrDatabase :: ByteString -> IO (StrDatabase s)
loadStrDatabase str =
  newStrDatabaseFrom (decode (ByteString.fromStrict str))
  
saveStrDatabase :: StrDatabase s -> IO ByteString
saveStrDatabase db =
  ByteString.toStrict <$> encode <$> diskContentsFrom <$> databaseContents db

-- Get all strings stored in the database.
databaseContents :: StrDatabase s -> IO [ByteString]
databaseContents (StrDatabase ref) = do
  contents@Contents{..} <- readIORef ref
  return
    [ uninternContents contents (Str n)
    | n <- [0..mc_next contents_memory-1] ]

-- Build a DiskContents from the list of interned strings.
diskContentsFrom :: [ByteString] -> DiskContents
diskContentsFrom strs =
  DiskContents {
    dc_string = ByteString.concat strs,
    dc_offsets = Vector.fromList offsets,
    dc_lengths = Vector.fromList lengths,
    dc_sorted = Vector.fromList sorted }
  where
    lengths = map ByteString.length strs
    offsets = init $ scanl (+) 0 lengths
    sorted =
      map fst $
      List.sortBy (comparing snd) $
      zip [0..] strs

-- A little test script of sorts.
test :: IO ()
test = do
  let
    check msg True = putStrLn ("PASSED: " ++ msg)
    check msg False = do
      putStrLn ("FAILED: " ++ msg)
      error "test failed"

    checks db xs = do
      check "intern returns correct string" $
        map (unintern db . intern db) xs == xs
      check "intern only adds strings once" $
        map (intern db) xs == map Str [0..List.genericLength xs-1]

  db <- newStrDatabase :: IO (StrDatabase ())
  let xs = ["hello", "world", "привет", "apa"]
  checks db xs

  -- Save, load and do some more operations.
  bs <- saveStrDatabase db
  db <- loadStrDatabase bs :: IO (StrDatabase ())
  let ys = xs ++ ["monkey", "banana", "zebra"]
  checks db ys

  -- Save again. Tests that saving a database which has both
  -- disk and memory contents works.
  bs <- saveStrDatabase db
  db@(StrDatabase ref) <- loadStrDatabase bs :: IO (StrDatabase ())
  checks db ys

  -- Check that if the database doesn't change, dumping and
  -- restoring is idempotent.
  bs <- saveStrDatabase db
  StrDatabase ref' <- loadStrDatabase bs :: IO (StrDatabase ())
  contents <- readIORef ref
  contents' <- readIORef ref'
  check "final database unchanged" (contents == contents')

  -- Print out the final database for eyeballing.
  putStrLn ("Final database: " ++ show contents)
