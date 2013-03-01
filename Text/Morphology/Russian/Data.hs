{-# LANGUAGE OverloadedStrings #-}
module Text.Morphology.Russian.Data ( Prefix(..)
                  , Ancode(..)
                  , FlexiaModel(..)
                  , Flexia(..)
                  , Lemma(..)
                  , FlMap
                  , PMap
                  , LMap
                  , Morph(..)
                  , FMap
                  , Res(..)
                  , KeyFlexia
                  , AncodeId
                  , BText(..)
                  ) where
import Data.Text (Text, unpack, pack, concat)
import Prelude hiding (concat)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (liftM)
import Data.Text.Encoding ( decodeUtf8, encodeUtf8)

type Pref = Text 
type Suffix = Text 
type AncodeId = Int
type PosId = Int
type GrammemId = Int
data Ancode = Ancode { idA::Int, posId::PosId, grammem::[GrammemId] } deriving (Show, Eq, Ord)
data Flexia = Flexia { prefixF::Pref, suffix::Suffix, ancode::AncodeId } deriving (Eq, Ord)
data FlexiaModel = FlexiaModel { idF::Int, flexias::[Flexia]} deriving (Eq, Ord)
data Prefix = Prefix { idP::Int, prefixP::Pref } deriving ( Eq, Ord)
data Lemma = Lemma {prefix::Maybe Int, flexia::Int, baseb::Text} deriving ( Eq, Ord)
 
type KeyFlexia = (Pref,Suffix)
type FlMap = Map.Map KeyFlexia [AncodeId]
type FMap = Map.Map Int FlMap
type LMap = Map.Map Text [Int]
type PMap = Map.Map Suffix (Set.Set Pref)
data Res = Res (Text,FlMap)
newtype BText = B Text

data Morph = Morph {lemmasM::LMap, flexiasM::FMap, prefixesM::PMap }

instance Binary Morph where
    put (Morph lm fm pm) = put (lm, fm, pm)
    get = do
        (lm, fm, pm) <- get
        return $ Morph lm fm pm

instance Show Flexia where
    show a = unpack $ concat [prefixF a, " ", suffix a, " ", pack $ show $ ancode a]
    
    
instance Show Prefix where
    show a = unpack $ concat [pack $ show $ idP a, " ", prefixP a]
    
instance Show Lemma where
    show a = case prefix a of
                  Nothing -> unpack $ concat [baseb a, " ", pack $ show $ flexia a]
                  Just b -> unpack $ concat [baseb a, " ", pack $ show $ flexia a, " ", pack $ show b]
                  
                
instance Show Res where
    show (Res (p,s)) = concatMap (\(x,y) -> unpack x ++ unpack p ++ unpack y ++ " ") (Map.keys s)

instance Binary Text where 
   put = put . encodeUtf8
   get = liftM decodeUtf8 get


