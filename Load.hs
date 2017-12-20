{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
import Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Strings
import Vector
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable(Vector)
import System.Environment
import qualified Data.ByteString as ByteString
import Corpus
import Data.Ord
import Data.Maybe
import System.Directory
import Data.Vector.Storable.ByteString
import Data.List.Split

type Sentence s = [Lexeme s]

toTokens :: StrDatabase s -> Element -> [Lexeme s]
toTokens db el
  | elName el `elem` [s, mw, corr, hi, trunc] =
    concatMap (toTokens db) (elChildren el)
  | elName el `elem` [pb, event, pause, shift, vocal, align] =
    []
  | elName el `elem` [gap, unclear] =
    [Gap]
  | elName el == c =
    [Punctuation {
      lexeme_text = intern db (strContent el),
      lexeme_claws_tag = find c5 }]
  | elName el == w =
    [Word {
      lexeme_lemma = find hw,
      lexeme_text = intern db (strContent el),
      lexeme_pos = find pos,
      lexeme_claws_tag = find c5 }]
  | otherwise =
    error $ "Unrecognised token: " ++ show el
  where
    s = unqual "s"
    mw = unqual "mw"
    gap = unqual "gap"
    corr = unqual "corr"
    pb = unqual "pb"
    hi = unqual "hi"
    pause = unqual "pause"
    vocal = unqual "vocal"
    align = unqual "align"
    shift = unqual "shift"
    event = unqual "event"
    trunc = unqual "trunc"
    unclear = unqual "unclear"
    c = unqual "c"
    w = unqual "w"
    c5 = unqual "c5"
    pos = unqual "pos"
    hw = unqual "hw"
    find x = intern db (fromJust (findAttr x el))

sentences :: StrDatabase s -> Element -> [Sentence s]
sentences db =
  map (toTokens db) . filterElements p
  where
    p x = elName x == unqual "s"

parse :: StrDatabase s -> T.Text -> [Sentence s]
parse db =
  sentences db . fromMaybe (error "parse error") . parseXMLDoc

main = do
  files <- getArgs
  createDirectoryIfMissing False "data"
  newStrDatabase >>= saveStrDatabase >>= ByteString.writeFile stringsFile

  -- We write the main data file incrementally, to save memory
  writeFile sentenceIndexFile ""

  let
    process !_ n [] = return n
    process db n (file:files) = do
      putStrLn ("Loading " ++ file ++ "...")
      sentences <- parse db <$> T.readFile file
      let
        tokens =
          Vector.fromList
          [ Token { token_sentence = i, token_position = j, token_lexeme = lexeme }
          | (i, sentence) <- zip [n..] sentences,
            (j, lexeme) <- zip [0..] sentence ]

      ByteString.appendFile sentenceIndexFile (vectorToByteString tokens)

      process db (n+length sentences) files

    -- We save and reload the string database every 20 files, to save memory
    processChunks !_ [] = return ()
    processChunks n (files:filess) = do
      db <- ByteString.readFile stringsFile >>= loadStrDatabase
      m <- process db n files
      putStrLn "Saving string database..."  
      saveStrDatabase db >>= ByteString.writeFile stringsFile
      processChunks m filess
    
  processChunks 0 (chunksOf 20 files)

  putStrLn "Generating index..."
  sentenceIndex <- readData sentenceIndexFile :: IO (Vector (Token ()))
  let
    comp Token{..} =
      (lexeme_maybe_lemma token_lexeme, token_position, token_sentence)
  
  lemmaIndex <-
    dropWhileMonotone NoGuess (isNothing . lexeme_maybe_lemma . token_lexeme) <$>
    sortBy (comparing comp) sentenceIndex
  writeData lemmaIndexFile lemmaIndex

