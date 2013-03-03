module Text.Morphology.Russian.Query where 

import Text.Morphology.Russian.Data 
import Text.Morphology.Russian.Parser 
import Data.Text (Text, pack, inits, tails, empty, length, drop, take)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Prelude hiding (length, take, drop)
import Control.Monad (liftM)
import Paths_rumorphology 

splitLemmFlex::Text -> [(Text,Text)]
splitLemmFlex a = zip (inits a) (tails a)


makeMorph::IO ()
makeMorph = do
    path <- getDataFileName "ru_RU-nojo.xml"
    (_,f,l) <- parse' path
    let morph = Morph (makeLMap l) (makeFMap f) (makePrefixMap $ concatMap flexias f)
    dir <- getDataDir
    L.writeFile (dir ++ "/morph.bin") $ encode morph
    where
      makeFMap xs = Map.fromList $ map (\(FlexiaModel i f) -> (i,makeFlMap f)) xs
      
morphBase ::  IO Morph
morphBase = do
    morph <- getDataFileName "morph.bin"
    liftM decode $ L.readFile morph

insertFlexia::Flexia -> FlMap -> FlMap
insertFlexia flexia' = Map.insertWith f key value 
                         where f x y = x ++ y
                               key = (prefixF flexia', suffix flexia')
                               value = [ancode flexia']
    
makeFlMap::[Flexia] -> FlMap
makeFlMap = foldr insertFlexia Map.empty 

makePrefixMap::[Flexia] -> PMap
makePrefixMap = foldr insertPrefix Map.empty 

insertPrefix::Flexia -> PMap -> PMap
insertPrefix flexia' = Map.insertWith Set.union key value 
                        where key = suffix flexia'
                              p = prefixF flexia'
                              e = pack ""
                              value = if p == e then Set.empty else Set.singleton p
      

makeLMap::[Lemma] -> LMap
makeLMap = foldr insertLemma Map.empty
               

insertLemma::Lemma -> LMap -> LMap
insertLemma l = Map.insertWith (++) key value 
                    where key = baseb l
                          value = [flexia l]
                          
word::Text -> Morph -> [Res]
word query (Morph lemmas flexiass prefixes) = [ Res (l,fM x)
                                        | (p,l,s) <- concat $ variant (splitLemmFlex query) prefixes
                                        , x <-  fromMaybe [] $ Map.lookup l lemmas
                                        , Map.member (p, s) $ fM x
                                        ]
                                        where fM = getFlexiaMapById flexiass
                                              
variant::[(Text,Text)] -> PMap -> [[(Text,Text,Text)]]
variant [] _ = []
variant (x:xs) prefixes = variant' x prefixes : variant xs prefixes

variant'::(Text,Text) -> PMap -> [(Text,Text,Text)]
variant' (fs,s) prefixes = let preff = Set.toList $ fromMaybe Set.empty $ Map.lookup s prefixes
                           in case preff of
                                [] -> [(empty, fs,s)]
                                _ -> [ (x, normalPrefix x fs, s)
                                    | x <-  Set.toList $ fromMaybe (Set.singleton empty) $ Map.lookup s prefixes
                                    ]

normalPrefix::Text -> Text -> Text
normalPrefix p l | p == take (length p) l = drop (length p) l
                 | otherwise = l
                      
getFlexiaMapById :: FMap -> Int -> FlMap
getFlexiaMapById f i = fromMaybe Map.empty $ Map.lookup i f

normalWord::[Res] -> [Text]
normalWord  = map makeW                     
     where makeW x = T.concat [preff x, body x, suf x]
           body = fst . normalForm 
           preff = fst . snd . normalForm 
           suf = snd . snd . normalForm

normalForm ::  Res -> (Text,KeyFlexia)
normalForm (Res (p,m)) = (p,fst $ foldr getMin (head l) $ tail l)
                         where l = Map.toList m

getMin::(KeyFlexia, [AncodeId]) -> (KeyFlexia, [AncodeId]) -> (KeyFlexia, [AncodeId])
getMin x@(_,xa) y@(_,ya) = if minimum xa > minimum ya 
                                then y
                                else x
                         
          
                                  
check::Text -> (KeyFlexia, [AncodeId]) -> Bool
check s (k,_) = s == snd k
                                        
  
