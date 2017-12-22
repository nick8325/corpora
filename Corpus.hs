-- Types for the corpus itself.
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, FlexibleContexts, UndecidableInstances, RankNTypes, RecordWildCards, ConstraintKinds #-}
module Corpus(
  Corpus(..), WithCorpus, BNC, withCorpus, loadCorpus,
  stringsFile, sentenceIndexFile, lemmaIndexFile, posIndexFile,
  sentenceIndex, lemmaIndex, posIndex,
  Sentence, showSentence, getSentence,
  Token(..), Lexeme(..), lexeme_maybe_lemma, lexeme_maybe_pos,
  Lemma, Text, POS, ClawsTag, SentenceNumber, Position) where

import Strings
import Index
import Vector
import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import Data.Reflection
import qualified Data.ByteString as ByteString
import qualified Data.Vector.Storable as Vector

data Corpus =
  Corpus {
    corpus_strings :: !(StrDatabase BNC),
    corpus_by_sentence :: Index (SentenceNumber, (Position, ())) Token,
    corpus_by_lemma :: Index (Lemma, (Position, (SentenceNumber, ()))) Token,
    corpus_by_pos :: Index (POS, (Position, (SentenceNumber, ()))) Token }
data BNC

sentenceIndex :: Given Corpus => Index (SentenceNumber, (Position, ())) Token
sentenceIndex = corpus_by_sentence given

lemmaIndex :: Given Corpus => Index (Lemma, (Position, (SentenceNumber, ()))) Token
lemmaIndex = corpus_by_lemma given

posIndex :: Given Corpus => Index (POS, (Position, (SentenceNumber, ()))) Token
posIndex = corpus_by_pos given

type WithCorpus = (Given Corpus, Given (StrDatabase BNC))
withCorpus :: Corpus -> (WithCorpus => a) -> a
withCorpus corpus x =
  give corpus (give (corpus_strings corpus) x)

loadCorpus :: IO Corpus
loadCorpus = do
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
  byPOS <-
    collate (lexeme_pos . token_lexeme) .
    collate token_position .
    collate token_sentence .
    index <$> readData posIndexFile
  return (Corpus strings bySentence byLemma byPOS)

stringsFile, sentenceIndexFile, lemmaIndexFile :: FilePath
stringsFile = "data/strings"
sentenceIndexFile = "data/by-sentence"
lemmaIndexFile = "data/by-lemma"
posIndexFile = "data/by-pos"

-- A sentence is a list of lexemes.
type Sentence = [Lexeme]

showSentence :: WithCorpus => Sentence -> String
showSentence xs = do
  x <- xs
  case x of
    Gap -> "<gap> "
    _ -> strValue (lexeme_text x)

getSentence :: WithCorpus => SentenceNumber -> Sentence
getSentence n =
  map token_lexeme (Vector.toList (get (sentenceIndex ! n)))

-- A token is a lexeme at a particular position in the corpus.
data Token =
  Token {
    token_sentence :: {-# UNPACK #-} !SentenceNumber,
    token_position :: {-# UNPACK #-} !Position,
    token_lexeme :: !Lexeme }
  deriving (Eq, Generic)

deriving instance Given (StrDatabase BNC) => Show Token

-- A lexeme is either a word, a punctuation mark, or a
-- gap (a place where the text is missing).
data Lexeme =
  Word {
    lexeme_lemma :: {-# UNPACK #-} !Lemma,
    lexeme_text :: {-# UNPACK #-} !Text,
    lexeme_pos :: {-# UNPACK #-} !POS,
    lexeme_claws_tag :: {-# UNPACK #-} !ClawsTag } |
  Punctuation {
    lexeme_text :: {-# UNPACK #-} !Text,
    lexeme_claws_tag :: {-# UNPACK #-} !ClawsTag } |
  Gap
  deriving (Eq, Generic)

lexeme_maybe_lemma :: Lexeme -> Maybe Lemma
lexeme_maybe_lemma Word{..} = Just lexeme_lemma
lexeme_maybe_lemma _ = Nothing

lexeme_maybe_pos :: Lexeme -> Maybe POS
lexeme_maybe_pos Word{..} = Just lexeme_pos
lexeme_maybe_pos _ = Nothing

deriving instance Given (StrDatabase BNC) => Show Lexeme

type SentenceNumber = Int
type Position = Int
type Lemma = Str BNC
type Text = Str BNC
type POS = Str BNC
type ClawsTag = Str BNC

----------------------------------------------------------------------
-- Storable instances.
----------------------------------------------------------------------

fromToken :: Token -> (Int, Int, Lexeme)
fromToken (Token x y z) = (x, y, z)

toToken :: (Int, Int, Lexeme) -> Token
toToken (x, y, z) = Token x y z

fromLexeme :: Lexeme -> (Str BNC, Str BNC, Str BNC, Str BNC)
fromLexeme (Word a b c d) = (a, b, c, d)
fromLexeme (Punctuation a b) = (invalidStr, invalidStr, a, b)
fromLexeme Gap = (invalidStr, invalidStr, invalidStr, invalidStr)

toLexeme :: (Str BNC, Str BNC, Str BNC, Str BNC) -> Lexeme
toLexeme (a, b, c, d)
  | a == invalidStr && b == invalidStr && c == invalidStr && d == invalidStr =
    Gap
  | a == invalidStr && b == invalidStr =
    Punctuation c d
  | otherwise = Word a b c d

-- Assumes that interned string IDs are always >= 0
invalidStr :: Str s
invalidStr = Str (-1)

instance Storable Token where
  sizeOf = sizeOf . fromToken
  alignment = alignment . fromToken
  peek ptr = toToken <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromToken x)

instance Storable Lexeme where
  sizeOf = sizeOf . fromLexeme
  alignment = alignment . fromLexeme
  peek ptr = toLexeme <$> peek (castPtr ptr)
  poke ptr x = poke (castPtr ptr) (fromLexeme x)
