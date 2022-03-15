{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
import Xeno.DOM
import Strings
import Vector
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable(Vector)
import System.Environment
import Data.ByteString(ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as ByteString
import Corpus
import Data.Ord
import Data.Maybe
import System.Directory
import Data.Vector.Storable.ByteString
import Data.List(intercalate)
import Data.List.Split

parseSentence :: StrDatabase BNC -> Node -> [Lexeme]
parseSentence db el
  | name el `elem` ["s", "mw", "corr", "hi", "trunc"] =
    concatMap (parseSentence db) (children el)
  | name el `elem` ["pb", "event", "pause", "shift", "vocal", "align"] =
    []
  | name el `elem` ["gap", "unclear"] =
    [Gap]
  | name el == "c" =
    [Punctuation {
      lexeme_text = intern db (ByteString.toString (strContent el)),
      lexeme_claws_tag = find "c5" }]
  | name el == "w" =
    [Word {
      lexeme_lemma = find "hw",
      lexeme_text = intern db (ByteString.toString (strContent el)),
      lexeme_pos = find "pos",
      lexeme_claws_tag = find "c5" }]
  | otherwise =
    error $ "Unrecognised token: " ++ show el
  where
    find x = intern db (ByteString.toString (fromJust (lookup x (attributes el))))

strContent :: Node -> ByteString
strContent n = ByteString.concat (concatMap text (contents n))
  where
    text (Element _) = []
    text (Text x) = [x]
    text (CData x) = [x]

findElements :: ByteString -> Node -> [Node]
findElements x n
  | name n == x = [n]
  | otherwise = concatMap (findElements x) (children n)

parseSentences :: StrDatabase BNC -> Node -> [Sentence]
parseSentences db =
  map (parseSentence db) . findElements "s"

parseText :: StrDatabase BNC -> ByteString -> [Sentence]
parseText db x =
  case parse x of
    Left err -> error ("parse error: " ++ show err)
    Right x -> parseSentences db x

main = do
  files <- getArgs

  createDirectoryIfMissing False dataDir
  db <- newStrDatabase

  -- We write the main data file incrementally, to save memory
  writeFile sentenceIndexFile ""

  let
    process n [] = return ()
    process n (file:files) = do
      putStrLn ("Loading " ++ file ++ "...")
      sentences <- parseText db <$> ByteString.readFile file
      let
        tokens =
          Vector.fromList
          [ Token { token_sentence = i, token_position = j, token_lexeme = lexeme }
          | (i, sentence) <- zip [n..] sentences,
            (j, lexeme) <- zip [0..] sentence ]

      ByteString.appendFile sentenceIndexFile (vectorToByteString tokens)
      compactStrDatabase db

      process (n+length sentences) files
    
  process 0 files

  putStrLn "Saving string database..."
  saveStrDatabase db >>= ByteString.writeFile stringsFile

  sentenceIndex <- readData sentenceIndexFile :: IO (Vector Token)
  putStrLn "Filtering out boring tokens..."
  let boring = isNothing . lexeme_maybe_lemma . token_lexeme
  filtered <-
    dropWhileSorted NearStart boring <$> sortBy (comparing (not . boring)) sentenceIndex

  putStrLn "Generating lemma index..."
  lemmaIndex <- sortBy (comparing lemmaKey) filtered
  writeData lemmaIndexFile lemmaIndex

  putStrLn "Generating POS index..."
  posIndex <- sortBy (comparing posKey) filtered
  writeData posIndexFile posIndex
