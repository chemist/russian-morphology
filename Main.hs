{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad

import           Data.Binary
import           Data.ByteString.Lazy          (readFile, writeFile)
import           Data.ByteString.UTF8          (fromString, toString)
import           Data.DAWG.Static
import           Data.Monoid
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text                     as T
import           Data.Vector                   (map)
import qualified Data.Vector                   as V

import           Prelude                       hiding (lookup, map, readFile)
import qualified Prelude                       as P

import           Control.Monad.IO.Class        (liftIO)
import           Data
import           Data.Maybe
import           Data.Text.Encoding            (decodeUtf8)
import           Grammem
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe           (serveDirectory, serveFile)
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5              as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes   as A


initMorph :: IO Morph
initMorph = decode <$> readFile dawgDict

main :: IO ()
main = do
    m <- initMorph
    web m

getAllForm :: Morph -> Text -> V.Vector Text
getAllForm m s =
  case lookup (unpack s) (dict m) of
       Nothing -> throw $ ErrorCall "unknown word"
       Just x ->  map (showParadigm (tags m) s) $ map (form m) x
  where
  form (Morph t p _) i = p V.! i

getAllForm' :: Morph -> Text -> H.Html
getAllForm' m s =
  case lookup (unpack . T.toUpper $ s) (dict m) of
       Nothing -> H.div ! A.class_ "input" ! A.id "input" $ "Незнакомое слово"
       Just x -> showP (tags m) (T.toUpper s) $ map (form m) x
  where
  form (Morph t p _) i = p V.! i

    
    
showP :: Tegs -> Text -> V.Vector TParadigm -> H.Html
showP tegs s vtp = tabs tegs s $ V.toList vtp

tabs :: Tegs -> Text -> [TParadigm] -> H.Html
tabs tegs s xs = do
    let contentByPosition = zip [1 .. ] xs
    H.section ! A.class_ "tabs" $ do
       nnInput $ P.map (nInput . fst) contentByPosition
       nnLabel contentByPosition
       H.div ! A.style "clear:both" $ ""
       H.div ! A.class_ "tabs_cont" $ do
           nnCont contentByPosition
    where
    nInput :: Int -> H.Html
    nInput x = H.input ! A.id (H.toValue $ "tab_" ++ show x) ! A.type_ "radio" ! A.name "tab" 
    nnInput :: [H.Html] -> H.Html
    nnInput (x:xs) = sequence_ ((x ! A.checked "checked") : xs)
    toName :: TParadigm -> H.Html
    toName x = do
        let lemma = toLemm s x
            (pref, suff, _) = head x
        H.toHtml $ pref <> lemma <> suff
    nLabel :: (Int, TParadigm) -> H.Html
    nLabel (x,y) = H.label ! A.for (H.toValue $ "tab_" ++ show x) ! A.id (H.toValue $ "tab_l" ++ show x) $ toName y
    nnLabel :: [(Int, TParadigm)] -> H.Html
    nnLabel xs = sequence_ $ P.map nLabel xs
    nCont (x,y) = H.div ! A.id (H.toValue $ "tab_c" ++ show x) $ paradigmToHtml tegs s y
    nnCont xs = sequence_ $ P.map nCont xs

spm :: Tegs -> Text -> (Text, Text, Int) -> H.Html
spm tegs w (pref, suff, t) = do
    H.div ! A.class_ "form" $ H.toHtml $ pref <> w <> suff
    H.div ! A.class_ "description" $ H.toHtml $ showTegs tegs t
    
showParadigm :: Tegs -> Text -> TParadigm -> Text
showParadigm tegs w xs = let lemma = toLemm w xs
                         in foldl (\x y -> showParadigm' tegs lemma y <> x) "" xs

paradigmToHtml :: Tegs -> Text -> TParadigm -> H.Html
paradigmToHtml tegs w xs = do
    let lemma = toLemm w xs
    H.ul $ forM_ xs  (\x -> H.li $ spm tegs lemma x)



showParadigm' :: Tegs -> Text -> (Text, Text, Int) -> Text
showParadigm' tegs w (pref, suff, t) = pref <> w <> suff <> " \t" <> showTegs tegs t <> "\n"

showTegs :: Tegs -> Int -> Text
showTegs tegs i = foldl fun "" $ tegs V.! i
  where fun x y = showFull y <> " ," <> x


toLemm :: Text -> TParadigm -> Text
toLemm s p = let (pref, suff, _) = maximum $ filter (\(x, y, _) -> T.take (T.length x) s == x && T.drop (T.length s - T.length y) s == y ) p
             in T.drop (T.length pref) $ T.take (T.length s - T.length suff) s


web :: Morph -> IO ()
web m = quickHttpServe $ ifTop (writeLazyText $ H.renderHtml $ indexPage usage) <|>
  route [ ("all"   , allForm m)
        , ("static", serveDirectory "static" )
        ]

normalForm = undefined

allForm :: Morph -> Snap ()
allForm m = do
    w <- getParam "word"
    writeLazyText $ H.renderHtml $ indexPage $ getAllForm' m $ decodeUtf8 $ fromJust w



indexPage :: H.Html -> H.Html
indexPage body = H.docTypeHtml $ do
    H.head $ do
        H.title "Морфология"
        H.link ! A.type_ "text/css" ! A.rel "stylesheet" ! A.href "static/css/style.css"
    H.body $ do
        H.form ! A.class_ "input" ! A.id "input" $ do
            H.input ! A.name "word"
            H.button "all" ! A.formaction "all"
        H.br
        body

usage :: H.Html
usage = H.div ! A.class_ "input" ! A.id "input" $  "Наберите слово и нажмите кнопку"

