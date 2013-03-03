{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Text.Morphology.Russian.Parser (
parse'
) where

import Control.Monad.Trans.Resource (MonadThrow)
import Data.Conduit (($$), Consumer)
import Text.XML.Stream.Parse ( tagNoAttr, many, tagPredicate
                             , requireAttr, tagName, ignoreAttrs
                             , optionalAttr, parseLBS, def, force)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.Text (unpack, toLower)
import Control.Monad.Reader (liftM, liftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.XML.Types (Event)

import Text.Morphology.Russian.Data (Prefix(..), Ancode(..), FlexiaModel(..), Flexia(..), Lemma(..))


parserPrefixes:: MonadThrow m => Consumer Event m (Maybe [Maybe Prefix])
parserPrefixes = tagNoAttr "prefixes" $ many $ tagPredicate (=="prefix_model") (requireAttr "id") $ \xid -> 
    tagPredicate (=="prefix") (requireAttr "value") $ \v -> 
       return $ Prefix (read (unpack xid)::Int) (toLower v)


parserOption:: MonadThrow m => Consumer Event m (Maybe (Maybe ()))
parserOption = tagNoAttr "options" $ tagName "locale" ignoreAttrs $ \_ -> return ()

parserPoses:: MonadThrow m => Consumer Event m (Maybe [()])
parserPoses = tagNoAttr "poses" $ many $ tagPredicate (=="pos") ignoreAttrs $ \_ -> return ()
    
parserGrammems:: MonadThrow m => Consumer Event m (Maybe [()])
parserGrammems = tagNoAttr "grammems" $ many $ tagPredicate (=="grammem") ignoreAttrs $ \_ -> return ()

parserAncodes:: MonadThrow m => Consumer Event m (Maybe [Ancode])
parserAncodes = tagNoAttr "ancodes" $ many $ tagPredicate (=="ancode") (do
    i <- requireAttr "id"
    requireAttr "name"
    p <- requireAttr "pos_id"
    return (i,p)
    ) $ \(i,p) -> do
        g <- grammemId
        return $ Ancode (read (unpack i)::Int) (read (unpack p)::Int) g
        
grammemId:: MonadThrow m => Consumer Event m [Int]
grammemId = many $ tagPredicate (== "grammem") (requireAttr "id") $ \x -> return (read (unpack x)::Int)
        
parserFlexiaModel :: MonadThrow m => Consumer Event m (Maybe [FlexiaModel])
parserFlexiaModel = tagNoAttr "flexias" $ many $ tagPredicate (=="flexia_model") (requireAttr "id") $ \xid -> do
    flexias' <- parserFlexias
    return $ FlexiaModel (read (unpack xid)::Int) flexias'

parserFlexias:: MonadThrow m => Consumer Event m [Flexia]
parserFlexias = many $ tagPredicate (=="flexia") ( do
    a <- requireAttr "prefix"
    b <- requireAttr "suffix"
    c <- requireAttr "ancode_id"
    return (a, b, c)) $ \(a, b, c) -> return $ Flexia (toLower a) (toLower b) (read (unpack c)::Int)
    
parserLemmas:: MonadThrow m => Consumer Event m (Maybe [Lemma])
parserLemmas = tagNoAttr "lemmas" $ many $ tagPredicate (=="lemma") (do
    a <- requireAttr "base"
    b <- requireAttr "flexia_id"
    c <- optionalAttr "prefix_id"
    optionalAttr "ancode_id"
    return (a,b,c))
    $ \(a,b,c) -> return $ Lemma (liftM normal c) (read (unpack b)::Int) (toLower a)
    where normal x = read (unpack x)::Int

parserBase:: MonadThrow m => Consumer Event m (Maybe ([Ancode], [Prefix], [FlexiaModel], [Lemma]))
parserBase = tagNoAttr "phpmorphy" $ do
    parserOption
    parserPoses
    parserGrammems
    ancodes <- parserAncodes
    f <- parserFlexiaModel
    a <- parserPrefixes
    l <- parserLemmas
    return (fromJust ancodes,map fromJust $ fromJust a, fromJust f, fromJust l)
   


    
parse':: (MonadIO m, MonadThrow m) => String -> m ([Prefix], [FlexiaModel], [Lemma])
parse' path = do
     lbs <- liftIO $ L.readFile path
     (_,p,f,l) <- parseLBS def lbs $$ force "bad" parserBase
     return (p,f,l)
   
