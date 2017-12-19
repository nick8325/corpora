-- Types for the corpus itself.
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes, RecordWildCards #-}
module Corpus(
  Token(..), Lexeme(..), lexeme_maybe_lemma,
  sentenceIndex, lemmaIndex,
  withCorpus, openCorpus,
  stringsFile, sentenceIndexFile, lemmaIndexFile) where

import Strings
import Index
import Vector
import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import Data.Reflection
import qualified Data.ByteString as ByteString

data Corpus s =
  Corpus {
    corpus_strings :: !(StrDatabase s),
    corpus_by_sentence :: Index (SentenceNumber, (Position, ())) (Token s),
    corpus_by_lemma :: Index (Lemma s, (Position, (SentenceNumber, ()))) (Token s) }

sentenceIndex :: Given (Corpus s) => Index (SentenceNumber, (Position, ())) (Token s)
sentenceIndex = corpus_by_sentence given

lemmaIndex :: Given (Corpus s) => Index (Lemma s, (Position, (SentenceNumber, ()))) (Token s)
lemmaIndex = corpus_by_lemma given

withCorpus :: Corpus s -> ((Given (Corpus s), Given (StrDatabase s)) => a) -> a
withCorpus corpus x =
  give corpus (give (corpus_strings corpus) x)

openCorpus :: IO (Corpus s)
openCorpus = do
  strings <- ByteString.readFile stringsFile >>= loadStrDatabase
  bySentence <-
    collate token_sentence .
    collate token_position .
    index <$> readData sentenceIndexFile
  byLemma <-
    collate (lexeme_lemma . token_lexeme) .
    collate token_position .
    collate token_sentence .
    index <$> readData lemmaIndexFile
  return (Corpus strings bySentence byLemma)

stringsFile, sentenceIndexFile, lemmaIndexFile :: FilePath
stringsFile = "data/strings"
sentenceIndexFile = "data/by-sentence"
lemmaIndexFile = "data/by-lemma"

-- A token is a lexeme at a particular position in the corpus.
data Token s =
  Token {
    token_sentence :: {-# UNPACK #-} !SentenceNumber,
    token_position :: {-# UNPACK #-} !Position,
    token_lexeme :: !(Lexeme s) }
  deriving (Eq, Generic)

deriving instance Given (StrDatabase s) => Show (Token s)

-- A lexeme is either a word, a punctuation mark, or a
-- gap (a place where the text is missing).
data Lexeme s =
  Word {
    lexeme_lemma :: {-# UNPACK #-} !(Lemma s),
    lexeme_text :: {-# UNPACK #-} !(Text s),
    lexeme_pos :: {-# UNPACK #-} !(POS s),
    lexeme_claws_tag :: {-# UNPACK #-} !(ClawsTag s) } |
  Punctuation {
    lexeme_text :: {-# UNPACK #-} !(Text s),
    lexeme_claws_tag :: {-# UNPACK #-} !(ClawsTag s) } |
  Gap
  deriving (Eq, Generic)

lexeme_maybe_lemma :: Lexeme s -> Maybe (Lemma s)
lexeme_maybe_lemma Word{..} = Just lexeme_lemma
lexeme_maybe_lemma _ = Nothing

deriving instance Given (StrDatabase s) => Show (Lexeme s)

type SentenceNumber = Int
type Position = Int
type Lemma s = Str s
type Text s = Str s
type POS s = Str s
type ClawsTag s = Str s

----------------------------------------------------------------------
-- Storable instances.
----------------------------------------------------------------------

fromToken :: Token s -> (Int, Int, Lexeme s)
fromToken (Token x y z) = (x, y, z)

toToken :: (Int, Int, Lexeme s) -> Token s
toToken (x, y, z) = Token x y z

fromLexeme :: Lexeme s -> (Str s, Str s, Str s, Str s)
fromLexeme (Word a b c d) = (a, b, c, d)
fromLexeme (Punctuation a b) = (invalidStr, invalidStr, a, b)
fromLexeme Gap = (invalidStr, invalidStr, invalidStr, invalidStr)

toLexeme :: (Str s, Str s, Str s, Str s) -> Lexeme s
toLexeme (a, b, c, d)
  | a == invalidStr && b == invalidStr && c == invalidStr && d == invalidStr =
    Gap
  | a == invalidStr && b == invalidStr =
    Punctuation c d
  | otherwise = Word a b c d

-- Assumes that interned string IDs are always >= 0
invalidStr :: Str s
invalidStr = Str (-1)

instance Storable (Token s) where
  sizeOf = sizeOf . fromToken
  alignment = alignment . fromToken
  peek ptr = toToken <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromToken x)

instance Storable (Lexeme s) where
  sizeOf = sizeOf . fromLexeme
  alignment = alignment . fromLexeme
  peek ptr = toLexeme <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromLexeme x)
