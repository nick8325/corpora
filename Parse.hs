{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import System.FilePath.Glob
import Control.Monad
import Database.SQLite.Simple
--import Database.PostgreSQL.Simple
import Data.String
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Data.List
import Data.Symbol

data Token =
  Word {
    c5 :: {-# UNPACK #-} !Symbol,
    pos :: {-# UNPACK #-} !Symbol,
    headWord :: {-# UNPACK #-} !Symbol,
    text :: {-# UNPACK #-} !Symbol } |
  Char {
    c5 :: {-# UNPACK #-} !Symbol,
    text :: {-# UNPACK #-} !Symbol } |
  Gap
  deriving (Eq, Show)

maybeC5 Gap = Nothing
maybeC5 x = Just (c5 x)

maybePos x@Word{} = Just (pos x)
maybePos _ = Nothing

maybeHw x@Word{} = Just (headWord x)
maybeHw _ = Nothing

maybeText Gap = Nothing
maybeText x = Just (text x)

type Sentence = [Token]

toTokens :: Element -> [Token]
toTokens el
  | elName el `elem` [s, mw, corr, hi, trunc] =
    concatMap toTokens (elChildren el)
  | elName el `elem` [pb, event, pause, shift, vocal, align] =
    []
  | elName el `elem` [gap, unclear] =
    [Gap]
  | elName el == c =
    [Char {
      c5 = find c5,
      text = intern $ strContent el }]
  | elName el == w =
    [Word {
      c5 = find c5,
      pos = find pos,
      headWord = find hw,
      text = intern $ strContent el }]
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
    find x = intern (fromJust (findAttr x el))

sentences :: Element -> [Sentence]
sentences =
  map toTokens . filterElements p
  where
    p x = elName x == unqual "s"

parse :: T.Text -> [Sentence]
parse =
  sentences . fromMaybe (error "parse error") . parseXMLDoc

main = do
  conn <- open "corpora.db"
  --conn <- connectPostgreSQL "dbname='corpora'"
  forM_ ["words", "c5", "hw", "text", "pos"] $ \table ->
    execute_ conn (fromString ("DROP TABLE IF EXISTS " ++ table))

  files <- glob "Texts/**/*.xml"

  let
    extend1 f s x =
      case f x of
        Nothing -> s
        Just y -> Set.insert y s

    extend !(!c5set, !hwset, !posset, !textset) x =
      (extend1 maybeC5 c5set x,
       extend1 maybeHw hwset x,
       extend1 maybePos posset x,
       extend1 maybeText textset x)

    extendM sets file = do
      putStrLn file
      sentences <- parse <$> T.readFile file
      return $! foldl' extend sets (concat sentences)

  (c5set, hwset, posset, textset) <-
    foldM extendM (Set.empty, Set.empty, Set.empty, Set.empty) files
  
  let
    collect s =
      Map.fromList (zip (Set.toList s) [1..])

    c5s = collect c5set
    hws = collect hwset
    poss = collect posset
    texts = collect textset

    create name val = do
      execute_ conn $ fromString $
        "CREATE TABLE " ++ name ++ "(id INTEGER PRIMARY KEY, " ++ name ++ " TEXT UNIQUE NOT NULL)"
      withTransaction conn $
        executeMany conn
          (fromString $ "INSERT INTO " ++ name ++ "(" ++ name ++ ", id) VALUES (?, ?)")
          [(unintern x, n) | (x, n) <- Map.toList val]
      
  putStrLn "Creating C5 table..."
  create "c5" c5s
  putStrLn "Creating POS table..."
  create "pos" poss
  putStrLn "Creating headword table..."
  create "hw" hws
  putStrLn "Creating text table..."
  create "text" texts

  putStrLn "Creating word table..."
  execute_ conn $ fromString $ unlines [
    "CREATE TABLE IF NOT EXISTS words (",
    "text INTEGER REFERENCES text,",
    "c5 INTEGER REFERENCES c5,",
    "hw INTEGER REFERENCES hw,",
    "pos INTEGER REFERENCES pos,",
    "sentence_no INTEGER NOT NULL,",
    "word_no INTEGER NOT NULL,",
    "PRIMARY KEY (sentence_no, word_no))"]

  let
    process n file = do
      putStrLn file
      sentences <- parse <$> T.readFile file
      
      withTransaction conn $
        executeMany conn
          "INSERT INTO words(text, c5, hw, pos, sentence_no, word_no) VALUES (?, ?, ?, ?, ?, ?)"
          [ toTuple i j token
          | (i, tokens) <- zip [n..] sentences,
            (j, token) <- zip [1..] tokens ]

      return (n + length sentences)

    toTuple :: Int -> Int -> Token -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Int, Int)
    toTuple i j Word{c5 = c5, text = text, pos = pos, headWord = headWord } =
      (Just (texts Map.! text), Just (c5s Map.! c5), Just (hws Map.! headWord), Just (poss Map.! pos), i, j)
    toTuple i j Char{c5 = c5, text = text} =
      (Just (texts Map.! text), Just (c5s Map.! c5), Nothing, Nothing, i, j)
    toTuple i j Gap =
      (Nothing, Nothing, Nothing, Nothing, i, j)

  foldM process 1 files
