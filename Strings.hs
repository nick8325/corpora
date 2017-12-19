-- Interned strings, backed by an on-disk database.
{-# LANGUAGE DeriveGeneric, FlexibleContexts, UndecidableInstances, RecordWildCards, BangPatterns #-}
module Strings(
  Str(..), Database,
  newDatabase, loadDatabase, saveDatabase,
  intern, unintern) where

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
import Control.DeepSeq
import Vector
import Data.Binary
import Data.Vector.Binary
import qualified Data.List as List
import Data.Ord

-- An interned string is really just a number.
-- The phantom type parameter represents the particular database.
newtype Str s = Str Int deriving (Eq, Ord)

showStrNum :: Str s -> String
showStrNum (Str n) = show n
    
newtype Database s = Database (IORef Contents)

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
    mc_next     :: {-# UNPACK #-} !Int,
    -- Maps from strings to their numbers and back again.
    mc_intern   :: !(Map String Int),
    mc_unintern :: !(IntMap String) }
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

instance Given (Database s) => IsString (Str s) where
  fromString = intern given

instance Given (Database s) => Show (Str s) where
  show = show . unintern given

----------------------------------------------------------------------
-- Interning and uninterning.
----------------------------------------------------------------------

intern :: Database s -> String -> Str s
intern (Database ref) str =
  deepseq str $
  unsafeDupablePerformIO $
  atomicModifyIORef' ref $ \contents ->
    internContents contents str

internContents :: Contents -> String -> (Contents, Str s)
internContents contents@Contents{contents_disk = disk@DiskContents{..}, contents_memory = MemoryContents{..}} str =
  case Vector.toList (findMonotone NoGuess (readString disk) str dc_sorted) of
    [n] -> (contents, Str n)
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
                 mc_unintern = IntMap.insert n str mc_unintern } },
            Str n)
    _ -> error "duplicate interned strings"

unintern :: Database s -> Str s -> String
unintern (Database ref) !str =
  unsafeDupablePerformIO $ do
    contents <- readIORef ref
    return (uninternContents contents str)

uninternContents :: Contents -> Str s -> String
uninternContents Contents{contents_disk = disk@DiskContents{..}, contents_memory = MemoryContents{..}} (Str n)
  | n < Vector.length dc_offsets =
    readString disk n
  | otherwise =
    case IntMap.lookup n mc_unintern of
      Just str -> str
      Nothing -> error "unknown interned string"

-- Read the value of a string from the on-disk part of the database.
readString :: DiskContents -> Int -> String
readString DiskContents{..} n =
  ByteString.toString $
    ByteString.take (dc_lengths ! n) $
    ByteString.drop (dc_offsets ! n) dc_string

----------------------------------------------------------------------
-- Creating, loading and storing databases.
----------------------------------------------------------------------

newDatabase :: IO (Database s)
newDatabase =
  newDatabaseFrom
    DiskContents {
      dc_string = ByteString.empty,
      dc_offsets = Vector.empty,
      dc_lengths = Vector.empty,
      dc_sorted = Vector.empty }

newDatabaseFrom :: DiskContents -> IO (Database s)
newDatabaseFrom disk =
  Database <$>
  newIORef Contents {
    contents_disk = disk,
    contents_memory =
      MemoryContents {
        mc_next = Vector.length (dc_offsets disk),
        mc_intern = Map.empty,
        mc_unintern = IntMap.empty } }
        
loadDatabase :: ByteString -> IO (Database s)
loadDatabase str =
  newDatabaseFrom (decode (ByteString.fromStrict str))
  
saveDatabase :: Database s -> IO ByteString
saveDatabase db =
  ByteString.toStrict <$> encode <$> diskContentsFrom <$> databaseContents db

-- Get all strings stored in the database.
databaseContents :: Database s -> IO [String]
databaseContents (Database ref) = do
  contents@Contents{..} <- readIORef ref
  return
    [ uninternContents contents (Str n)
    | n <- [0..mc_next contents_memory-1] ]

-- Build a DiskContents from the list of interned strings.
diskContentsFrom :: [String] -> DiskContents
diskContentsFrom strs =
  DiskContents {
    dc_string = ByteString.concat bytes,
    dc_offsets = Vector.fromList offsets,
    dc_lengths = Vector.fromList lengths,
    dc_sorted = Vector.fromList sorted }
  where
    bytes = map ByteString.fromString strs
    -- N.B. must use bytes rather than strs here
    lengths = map ByteString.length bytes
    offsets = init $ scanl (+) 0 lengths
    -- N.B. must use strs rather than bytes here,
    -- because the ordering used in intern is that of String
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
        map (intern db) xs == map Str [0..length xs-1]

  db <- newDatabase :: IO (Database ())
  let xs = ["hello", "world", "привет", "apa"]
  checks db xs

  -- Save, load and do some more operations.
  bs <- saveDatabase db
  db <- loadDatabase bs :: IO (Database ())
  let ys = xs ++ ["monkey", "banana", "zebra"]
  checks db ys

  -- Save again. Tests that saving a database which has both
  -- disk and memory contents works.
  bs <- saveDatabase db
  db@(Database ref) <- loadDatabase bs :: IO (Database ())
  checks db ys

  -- Check that if the database doesn't change, dumping and
  -- restoring is idempotent.
  bs <- saveDatabase db
  Database ref' <- loadDatabase bs :: IO (Database ())
  contents <- readIORef ref
  contents' <- readIORef ref'
  check "final database unchanged" (contents == contents')

  -- Print out the final database for eyeballing.
  putStrLn ("Final database: " ++ show contents)
