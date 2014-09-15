{-# LANGUAGE ImplicitParams #-}
module Main where
import           Control.Applicative              hiding (empty)
import           Control.Exception
import           Control.Monad

import           System.Directory                 (doesFileExist)
import           System.IO                        hiding (readFile, writeFile)
import           System.IO.Streams                hiding (drop, filter, map,
                                                   maxIndex, decodeUtf8, minIndex, zip)
import qualified System.IO.Streams                as STR
import           System.IO.Streams.Attoparsec

import           Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
import           Data.Attoparsec.Text             (isEndOfLine)
import           Data.Binary                      (decode, encode)
import           Data.Binary                      (Binary, get, put)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as S
import           Data.ByteString.Char8            (pack, unpack)
import           Data.ByteString.Lazy             (readFile, writeFile)
import qualified Data.ByteString.Lazy             as L
import           Data.ByteString.UTF8             (toString, fromString)
import           Data.DAWG.Static                 (DAWG)
import qualified Data.DAWG.Static                 as D
import           Data.Encoding                    (decodeString, encodeString,
                                                   encodingFromString)
import           Data.Function                    (on)
import           Data.List                        (drop, maximumBy, scanl,
                                                   tails, union, zip)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Data.Vector                      (Vector, cons, elemIndex,
                                                   empty, maxIndex, minIndex,
                                                   (!))
import qualified Data.Vector                      as V
import           Data.Vector.Binary
import           Data.Word                        (Word8)

import           Prelude                          hiding (readFile, takeWhile,
                                                   writeFile, zip)

import           Data
import Grammem

--------------------------------- Types -----------------------------------------

type TagsSet = Set.Set Tag
type RuWord = [(ByteString, ParadigmNum)]
type RuWords = Vector (ByteString, ParadigmNum)
type MapTags = Map.Map Tag TagNum
type MapParadigms = Map.Map Paradigm ParadigmNum

-------------------------------- IO ----------------------------------------

main :: IO ()
main = do
    tagsBin <- doesFileExist tagsFile
    unless tagsBin $ do
        tags <- withFileAsInput dictFile parserTags
        writeFile tagsFile (encode tags)
    tags <- decode <$> readFile tagsFile :: IO Tags
    let tagsMap = Prelude.foldl  (\x y -> Map.insert (tags ! y) y x) Map.empty [0 .. V.length tags - 1]
    print "finish tags"

    paradigmsBin <- doesFileExist paradigmsFile
    unless paradigmsBin $ do
        paradigms <- withFileAsInput dictFile (parserParadigms tagsMap)
        writeFile paradigmsFile (encode paradigms)
    paradigms <- decode <$> readFile paradigmsFile :: IO Paradigms
    let paradigmsMap = Prelude.foldl  (\x y -> Map.insert (paradigms ! y) y x) Map.empty [0 .. V.length paradigms - 1]
    print "finish paradigms"

    wordBin <- doesFileExist wordsFile
    unless wordBin $ do
        result <- withFileAsInput dictFile (parserWords tagsMap paradigmsMap)
        writeFile wordsFile $! encode $! result
    words <- decode <$> readFile wordsFile :: IO [RuWord]
    print "finish words"

    let stwords = map (\(x,y) -> (toString x, V.singleton y)) $! concat words
        daw = D.fromListWith (\x y -> uniqV (x V.++ y)) stwords
    writeFile dawgDict $! encode $! Morph (tagsToTegs tags) (convertParadigms paradigms) daw -- dawgDict $! encode daw
    
uniqV :: Vector Int -> Vector Int
uniqV = V.fromList . Set.toList . Set.fromList . V.toList   

convertParadigms :: Paradigms -> TParadigms
convertParadigms = V.map fun 
  where fun = map (\(x,y,z) -> (decodeUtf8 x, decodeUtf8 y, z)) 

parserTags :: InputStream ByteString -> IO Tags
parserTags is = setToVector <$> fun
  where
  fun = fold (flip Set.insert) Set.empty =<< parserToInputStream parserTag is

parserParadigms :: MapTags -> InputStream ByteString -> IO Paradigms
parserParadigms tags is = setToVector <$> fun
  where
  fun = fold (flip Set.insert) Set.empty =<< parserToInputStream (parserParadigm tags) is

parserWords :: MapTags -> MapParadigms -> InputStream ByteString -> IO [RuWord]
parserWords mt mp is = STR.toList =<< parserToInputStream (parserWord mt mp) is

------------------------------------------ attoparsec parsers ----------------------------------------------

tagsToTegs :: Tags -> Vector [Teg]
tagsToTegs = V.map fun
  where 
  fun :: Tag -> [Teg]
  fun x = let Right y = parseOnly parseTegs x
          in y

withDigit :: Parser ByteString
withDigit = numOrLine *> withoutDigit

withoutDigit :: Parser ByteString
withoutDigit = takeWhile splitter *> satisfy tabulator *> takeTill isEndOfLine <* endOfLine

numOrLine :: Parser ()
numOrLine = (number *> endOfLine *> pure ()) <|> pure () <* many1 (endOfLine *> number *> endOfLine )

tabulator, splitter :: Char -> Bool
tabulator = inClass "\t "
splitter = notInClass "0-9\t\n"

parserTag :: Parser (Maybe Tag)
parserTag = (endOfLine *> endOfInput *> pure Nothing) <|> (Just <$> (withDigit <|> withoutDigit))

parserParadigm :: MapTags -> Parser (Maybe Paradigm)
parserParadigm tags = (endOfLine *> endOfInput *> pure Nothing) <|> (Just <$> parseParadigm)
  where
  parseParadigm = makeParadigm <$> (numOrLine *> (many parsePair))
  parsePair = do
      word <- takeWhile splitter <* satisfy tabulator
      tag <- takeTill isEndOfLine <* endOfLine
      return (word, tagNum tag tags)

parserWord :: MapTags -> MapParadigms -> Parser (Maybe RuWord)
parserWord tags paradigms = (endOfLine *> endOfInput *> pure Nothing) <|> (Just <$> parseWord)
  where parseWord = numOrLine *> (makeWords paradigms <$> (many pWords))
        pWords = do
            word <- takeWhile splitter <* satisfy tabulator
            tag <- takeTill isEndOfLine <* endOfLine
            return (word, tagNum tag tags)

--------------------------------------- Pure ----------------------------------------------------

makeParadigm :: [(ByteString, TagNum)] -> Paradigm
makeParadigm xs = let lemm = getLemm $ map fst xs
                      toP (x, y) t = (x, y, t)
                  in map (\(bs, t) -> toP (tokenise lemm bs) t) xs

tokenise :: ByteString -> ByteString -> (ByteString, ByteString)
tokenise x y = (h , (S.drop (S.length x) t))
           where (h,t) = S.breakSubstring x y

getLemm :: [ByteString] -> ByteString
getLemm (x:[]) = x
getLemm (x:y:[]) = lcb x y
getLemm (x:y:s) = foldl lcb (lcb x y) s

lcb :: ByteString -> ByteString -> ByteString
lcb xs ys = let f :: ByteString -> ByteString -> [ByteString]
                f a b = scanl g S.empty $ S.zip a b
                g :: ByteString -> (Word8, Word8) -> ByteString
                g z (x, y) = if x == y then S.snoc z x else S.empty
                rm x = if ((not (S.null x)) &&  (S.last x == 208 || S.last x == 209)) then S.init x else x
            in rm $ maximumBy (compare `on` S.length) . concat $ [f xs' ys | xs' <- S.tails xs] ++ [f xs ys' | ys' <- drop 1 $ S.tails ys]

makeWords :: MapParadigms -> [(ByteString, TagNum)] -> RuWord
makeWords paradigms pairs = let p = makeParadigm pairs
                                index = fromJustE ("make words" ++ show p) $ Map.lookup p paradigms
                            in map (\(x, _) -> (x, index)) pairs

tagNum :: ByteString -> MapTags -> TagNum
tagNum tag m = fromJustE ("tagnum'" ++ show tag) $ Map.lookup tag m

setToVector::Set.Set a -> Vector a
setToVector = V.fromList . Set.toList

fromJustE :: String -> Maybe a -> a
fromJustE msg Nothing = throw $ ErrorCall msg
fromJustE _ (Just x) = x


koi8 = encodingFromString "koi8-r"

fromKoi8 :: String -> String
fromKoi8 xs = let ?enc = koi8
              in decodeString ?enc xs

toKoi8 :: String -> String
toKoi8 xs = let ?enc = koi8
            in encodeString ?enc $ xs


