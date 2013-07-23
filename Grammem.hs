{-# LANGUAGE OverloadedStrings #-}
module Grammem where

import Control.Applicative
import Data.Attoparsec
import Data.Binary
import Data.Text
import Prelude hiding (drop)

data Teg = T'POST
         | T'NOUN
         | T'ADJF
         | T'ADJS
         | T'COMP
         | T'VERB
         | T'INFN
         | T'PRTF
         | T'PRTS
         | T'GRND
         | T'NUMR
         | T'ADVB
         | T'NPRO
         | T'PRED
         | T'PREP
         | T'CONJ
         | T'PRCL
         | T'INTJ
         | T'ANim
         | T'anim
         | T'inan
         | T'GNdr
         | T'masc
         | T'femn
         | T'neut
         | T'Msf
         | T'NMbr
         | T'sing
         | T'plur
         | T'Sgtm
         | T'Pltm
         | T'Fixd
         | T'CAse
         | T'nomn
         | T'gent
         | T'datv
         | T'accs
         | T'ablt
         | T'loct
         | T'voct
         | T'gen1
         | T'gen2
         | T'acc2
         | T'loc1
         | T'loc2
         | T'Abbr
         | T'Name
         | T'Surn
         | T'Patr
         | T'Geox
         | T'Orgn
         | T'Trad
         | T'Subx
         | T'Supr
         | T'Qual
         | T'Apro
         | T'Anum
         | T'Poss
         | T'Vey
         | T'Voy
         | T'Cmp2
         | T'Vej
         | T'ASpc
         | T'perf
         | T'impf
         | T'TRns
         | T'tran
         | T'intr
         | T'Impe
         | T'Uimp
         | T'Mult
         | T'Refl
         | T'PErs
         | T'1per
         | T'2per
         | T'3per
         | T'TEns
         | T'pres
         | T'past
         | T'futr
         | T'MOod
         | T'indc
         | T'impr
         | T'INvl
         | T'incl
         | T'excl
         | T'VOic
         | T'actv
         | T'pssv
         | T'Infr
         | T'Slng
         | T'Arch
         | T'Litr
         | T'Erro
         | T'Dist
         | T'Ques
         | T'Dmns
         | T'Prnt
         | T'Vbe
         | T'Ven
         | T'Vie
         | T'Vbi
         | T'Fimp
         | T'Prdx
         | T'Coun
         | T'Coll
         | T'Vsh
         | T'Afp
         | T'Inmx
         | T'Vpre
         deriving (Show, Eq, Ord, Enum)
         
instance Binary Teg where
  put x = put $ fromEnum x
  get = toEnum <$> get 

parseTegs :: Parser [Teg]
parseTegs = parseTeg `sepBy` anyWord8


parseTeg :: Parser Teg
parseTeg =   string "POST" *> pure  T'POST
         <|> string "NOUN" *> pure  T'NOUN
         <|> string "ADJF" *> pure  T'ADJF
         <|> string "ADJS" *> pure  T'ADJS
         <|> string "COMP" *> pure  T'COMP
         <|> string "VERB" *> pure  T'VERB
         <|> string "INFN" *> pure  T'INFN
         <|> string "PRTF" *> pure  T'PRTF
         <|> string "PRTS" *> pure  T'PRTS
         <|> string "GRND" *> pure  T'GRND
         <|> string "NUMR" *> pure  T'NUMR
         <|> string "ADVB" *> pure  T'ADVB
         <|> string "NPRO" *> pure  T'NPRO
         <|> string "PRED" *> pure  T'PRED
         <|> string "PREP" *> pure  T'PREP
         <|> string "CONJ" *> pure  T'CONJ
         <|> string "PRCL" *> pure  T'PRCL
         <|> string "INTJ" *> pure  T'INTJ
         <|> string "ANim" *> pure  T'ANim
         <|> string "anim" *> pure  T'anim
         <|> string "inan" *> pure  T'inan
         <|> string "GNdr" *> pure  T'GNdr
         <|> string "masc" *> pure  T'masc
         <|> string "femn" *> pure  T'femn
         <|> string "neut" *> pure  T'neut
         <|> string "Ms-f" *> pure  T'Msf
         <|> string "NMbr" *> pure  T'NMbr
         <|> string "sing" *> pure  T'sing
         <|> string "plur" *> pure  T'plur
         <|> string "Sgtm" *> pure  T'Sgtm
         <|> string "Pltm" *> pure  T'Pltm
         <|> string "Fixd" *> pure  T'Fixd
         <|> string "CAse" *> pure  T'CAse
         <|> string "nomn" *> pure  T'nomn
         <|> string "gent" *> pure  T'gent
         <|> string "datv" *> pure  T'datv
         <|> string "accs" *> pure  T'accs
         <|> string "ablt" *> pure  T'ablt
         <|> string "loct" *> pure  T'loct
         <|> string "voct" *> pure  T'voct
         <|> string "gen1" *> pure  T'gen1
         <|> string "gen2" *> pure  T'gen2
         <|> string "acc2" *> pure  T'acc2
         <|> string "loc1" *> pure  T'loc1
         <|> string "loc2" *> pure  T'loc2
         <|> string "Abbr" *> pure  T'Abbr
         <|> string "Name" *> pure  T'Name
         <|> string "Surn" *> pure  T'Surn
         <|> string "Patr" *> pure  T'Patr
         <|> string "Geox" *> pure  T'Geox
         <|> string "Orgn" *> pure  T'Orgn
         <|> string "Trad" *> pure  T'Trad
         <|> string "Subx" *> pure  T'Subx
         <|> string "Supr" *> pure  T'Supr
         <|> string "Qual" *> pure  T'Qual
         <|> string "Apro" *> pure  T'Apro
         <|> string "Anum" *> pure  T'Anum
         <|> string "Poss" *> pure  T'Poss
         <|> string "V-ey" *> pure  T'Vey
         <|> string "V-oy" *> pure  T'Voy
         <|> string "Cmp2" *> pure  T'Cmp2
         <|> string "V-ej" *> pure  T'Vej
         <|> string "ASpc" *> pure  T'ASpc
         <|> string "perf" *> pure  T'perf
         <|> string "impf" *> pure  T'impf
         <|> string "TRns" *> pure  T'TRns
         <|> string "tran" *> pure  T'tran
         <|> string "intr" *> pure  T'intr
         <|> string "Impe" *> pure  T'Impe
         <|> string "Uimp" *> pure  T'Uimp
         <|> string "Mult" *> pure  T'Mult
         <|> string "Refl" *> pure  T'Refl
         <|> string "PErs" *> pure  T'PErs
         <|> string "1per" *> pure  T'1per
         <|> string "2per" *> pure  T'2per
         <|> string "3per" *> pure  T'3per
         <|> string "TEns" *> pure  T'TEns
         <|> string "pres" *> pure  T'pres
         <|> string "past" *> pure  T'past
         <|> string "futr" *> pure  T'futr
         <|> string "MOod" *> pure  T'MOod
         <|> string "indc" *> pure  T'indc
         <|> string "impr" *> pure  T'impr
         <|> string "INvl" *> pure  T'INvl
         <|> string "incl" *> pure  T'incl
         <|> string "excl" *> pure  T'excl
         <|> string "VOic" *> pure  T'VOic
         <|> string "actv" *> pure  T'actv
         <|> string "pssv" *> pure  T'pssv
         <|> string "Infr" *> pure  T'Infr
         <|> string "Slng" *> pure  T'Slng
         <|> string "Arch" *> pure  T'Arch
         <|> string "Litr" *> pure  T'Litr
         <|> string "Erro" *> pure  T'Erro
         <|> string "Dist" *> pure  T'Dist
         <|> string "Ques" *> pure  T'Ques
         <|> string "Dmns" *> pure  T'Dmns
         <|> string "Prnt" *> pure  T'Prnt
         <|> string "V-be" *> pure  T'Vbe
         <|> string "V-en" *> pure  T'Ven
         <|> string "V-ie" *> pure  T'Vie
         <|> string "V-bi" *> pure  T'Vbi
         <|> string "Fimp" *> pure  T'Fimp
         <|> string "Prdx" *> pure  T'Prdx
         <|> string "Coun" *> pure  T'Coun
         <|> string "Coll" *> pure  T'Coll
         <|> string "V-sh" *> pure  T'Vsh
         <|> string "Af-p" *> pure  T'Afp
         <|> string "Inmx" *> pure  T'Inmx
         <|> string "Vpre" *> pure  T'Vpre


showAbr :: Teg -> Text
showAbr x | x == T'Msf = "Ms-f"
          | x == T'Vey = "V-ey"
          | x == T'Vej = "V-ej"
          | x == T'Voy = "V-oy"
          | x == T'Vbe = "V-be"
          | x == T'Ven = "V-en"
          | x == T'Vie = "V-ie"
          | x == T'Vbi = "V-bi"
          | x == T'Vsh = "V-sh"
          | x == T'Afp = "Af-p"
          | otherwise = drop 2 $ pack $ show x

showFull :: Teg -> Text
showFull T'POST	= "часть речи"
showFull T'NOUN	= "имя существительное"
showFull T'ADJF	= "имя прилагательное (полное)"
showFull T'ADJS	= "имя прилагательное (краткое)"
showFull T'COMP	= "компаратив"
showFull T'VERB	= "глагол (личная форма)"
showFull T'INFN	= "глагол (инфинитив)"
showFull T'PRTF	= "причастие (полное)"
showFull T'PRTS	= "причастие (краткое)"
showFull T'GRND	= "деепричастие"
showFull T'NUMR	= "числительное"
showFull T'ADVB	= "наречие"
showFull T'NPRO	= "местоимение-существительное"
showFull T'PRED	= "предикатив"
showFull T'PREP	= "предлог"
showFull T'CONJ	= "союз"
showFull T'PRCL	= "частица"
showFull T'INTJ	= "междометие"
showFull T'ANim	= "одушевлённость не выражена"
showFull T'anim	= "одушевлённое"
showFull T'inan	= "неодушевлённое"
showFull T'GNdr	= "род не выражен"
showFull T'masc	= "мужской род"
showFull T'femn	= "женский род"
showFull T'neut	= "средний род"
showFull T'Msf	= "общий род"
showFull T'NMbr	= "число"
showFull T'sing	= "единственное число"
showFull T'plur	= "множественное число"
showFull T'Sgtm	= "singularia tantum"
showFull T'Pltm	= "pluralia tantum"
showFull T'Fixd	= "неизменяемое"
showFull T'CAse	= "категория падежа"
showFull T'nomn	= "именительный падеж"
showFull T'gent	= "родительный падеж"
showFull T'datv	= "дательный падеж"
showFull T'accs	= "винительный падеж"
showFull T'ablt	= "творительный падеж"
showFull T'loct	= "предложный падеж"
showFull T'voct	= "звательный падеж"
showFull T'gen1	= "первый родительный падеж"
showFull T'gen2	= "второй родительный (частичный) падеж"
showFull T'acc2	= "второй винительный падеж"
showFull T'loc1	= "первый предложный падеж"
showFull T'loc2	= "второй предложный (местный) падеж"
showFull T'Abbr	= "аббревиатура"
showFull T'Name	= "имя"
showFull T'Surn	= "фамилия"
showFull T'Patr	= "отчество"
showFull T'Geox	= "топоним"
showFull T'Orgn	= "организация"
showFull T'Trad	= "торговая марка"
showFull T'Subx	= "возможна субстантивация"
showFull T'Supr	= "превосходная степень"
showFull T'Qual	= "качественное"
showFull T'Apro	= "местоименное"
showFull T'Anum	= "порядковое"
showFull T'Poss	= "притяжательное"
showFull T'Vey	= "форма на -ею"
showFull T'Voy	= "форма на -ою"
showFull T'Cmp2	= "сравнительная степень на по-"
showFull T'Vej	= "форма компаратива на -ей"
showFull T'ASpc	= "категория вида"
showFull T'perf	= "совершенный вид"
showFull T'impf	= "несовершенный вид"
showFull T'TRns	= "категория переходности"
showFull T'tran	= "переходный"
showFull T'intr	= "непереходный"
showFull T'Impe	= "безличный"
showFull T'Uimp	= "безличное употребление"
showFull T'Mult	= "многократный"
showFull T'Refl	= "возвратный"
showFull T'PErs	= "категория лица"
showFull T'1per	= "1 лицо"
showFull T'2per	= "2 лицо"
showFull T'3per	= "3 лицо"
showFull T'TEns	= "категория времени"
showFull T'pres	= "настоящее время"
showFull T'past	= "прошедшее время"
showFull T'futr	= "будущее время"
showFull T'MOod	= "категория наклонения"
showFull T'indc	= "изъявительное наклонение"
showFull T'impr	= "повелительное наклонение"
showFull T'INvl	= "категория совместности"
showFull T'incl	= "говорящий включён (идем, идемте)"
showFull T'excl	= "говорящий не включён в действие (иди, идите)"
showFull T'VOic	= "категория залога"
showFull T'actv	= "действительный залог"
showFull T'pssv	= "страдательный залог"
showFull T'Infr	= "разговорное"
showFull T'Slng	= "жаргонное"
showFull T'Arch	= "устаревшее"
showFull T'Litr	= "литературный вариант"
showFull T'Erro	= "опечатка"
showFull T'Dist	= "искажение"
showFull T'Ques	= "вопросительное"
showFull T'Dmns	= "указательное"
showFull T'Prnt	= "вводное слово"
showFull T'Vbe	= "форма на -ье"
showFull T'Ven	= "форма на -енен"
showFull T'Vie	= "отчество через -ие-"
showFull T'Vbi	= "форма на -ьи"
showFull T'Fimp	= "деепричастие от глагола несовершенного вида"
showFull T'Prdx	= "может выступать в роли предикатива"
showFull T'Coun	= "счётная форма"
showFull T'Coll	= "собирательное числительное"
showFull T'Vsh	= "деепричастие на -ши"
showFull T'Afp	= "форма после предлога"
showFull T'Inmx	= "может использоваться как одуш. / неодуш."
showFull T'Vpre	= "Вариант предлога ( со, подо, ...)"

