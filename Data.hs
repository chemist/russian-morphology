module Data where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (get, put, Binary)
import Data.Vector (Vector)
import Data.Vector.Binary ()
import Data.DAWG.Static (DAWG)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Binary

import Grammem

type TagNum = Int
type ParadigmNum = Int

type Tag = ByteString
type Prefix = ByteString
type Suffix = ByteString

type Tags = Vector Tag
type Paradigms = Vector Paradigm
type TParadigms = Vector TParadigm

type Tegs = Vector [Teg]

type Paradigm = [(Prefix, Suffix, TagNum)]
type TParadigm = [(Text, Text, TagNum)]

data Morph = Morph
  { tags      :: !Tegs
  , paradigms :: !TParadigms
  , dict      :: !(DAWG Char () (Vector ParadigmNum))
  } 
  
instance Binary Morph where
    put x = put (tags x) >> put (paradigms x) >> put (dict x)
    get = Morph <$> get <*> get <*> get
    
--------------------------------- const ----------------------------------------

dictFile, tagsFile, paradigmsFile, wordsFile :: String
dictFile = "dict.opcorpora.txt"
tagsFile = "tags.bin"
paradigmsFile = "paradigms.bin"
wordsFile = "words.bin"
dawgDict = "dawg.dict"
