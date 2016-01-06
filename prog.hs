{-# LANGUAGE FlexibleContexts #-}

import Data.List.Split
import Data.List as List
import Data.Map as Map
import qualified Data.Set as Set
import System.IO
import Data.Maybe(fromMaybe)
import Debug.Trace
{-
Laua seisundite loetavaks väljastamiseks
-}
fancyPrint [] = ""
fancyPrint (x:xs) =  (x ++ "\n") ++ (fancyPrint xs)

{-
Loendab mitmes laua reas bloki juppe ehk numbreid asub.
Kui tulemus on suurem kui üks, viitab see sellele, et blokk 
on läbi mitme rea ja seega vertikaalne. Kui tulemus on 1, siis
järelikult on blokk horisontaalne. Kui tulemus 0, siis sellist
blokki ei leidu.
-}
count [] _ = 0
count (x:xs) v
  | elem v x = 1 + count xs v
  | otherwise = 0 + count xs v

{-
Kontrollimaks kas kasutaja soovitud liigutus on
vastava bloki puhul lubatud. See liigutuse kontroll veidi
ebaefektiivsem kui arvuti mängimise puhul, kuna ei salvesta infot
listi. Samas kuna kasutaja puhul peab protsessima väheseid käske,
siis ei ole vaja efektiivsemat lahendust.
-}
checkMovement lines a s
  | (count lines a)>1 && s == 'u' = True
  | (count lines a)>1 && s == 'd' = True
  | (count lines a)==1 && s == 'l' = True
  | (count lines a)==1 && s == 'r' = True
  | otherwise = False
{-
Kontrollimaks kas kasutaja soovitud liigutus on
vastava bloki puhul lubatud. See liigutuse kontroll veidi
efektiivsem kui kasutaja mängimise puhul, kuna salvestab listi
ja iga kord ei pea uuesti infot hankima iga bloki asetuse kohta 
loendamise teel. Loob listi kujul ['0h','1v',...] kus h = horisontaalne
ja v = vertikaalne. Otherwise tingimust 'o' ei tohiks tekkida, see on
kontrolliks.
-}
checkMovementAI [] lines = []
checkMovementAI (x:xs) lines
  | (count lines x)>1 = ([x] ++ ['v']) : checkMovementAI xs lines
  | (count lines x)==1 = ([x] ++ ['h']) : checkMovementAI xs lines
  | otherwise = ([x] ++ ['o']) : checkMovementAI xs lines

{-
Kontrollib bloki lubatud liigutusi checkMovementAI poolt tehtud
listist.
-}
lookupAI [] a = 'o'
lookupAI (x:xs) a
  | x !! 0 == a = x !! 1
  | otherwise = lookupAI xs a

{-
Funktsioon, mis kontrollib kasutaja mängimist.
-}
play lines yGate a s c
    | c == False =do hSetBuffering stdin LineBuffering
                     putStr "Hetkeseis on selline: \n"
                     putStrLn(fancyPrint lines)
                     putStr "Palun liigutage klotse trükkides klaviatuurilt\nvastavad käsud (1r || 2r || (nr)(suund) )\n"
                     putStr "Suunad: u = üles, d = alla, r = paremale, l = vasakule\n\n"
                     movementinfo <- getLine
                     putStr "Sisse loetud!\n"
                     --linuxis sain tööle ilma sellise salvestamiseta
                     --vajalik windowsi jaoks vist siis
                     let k = head movementinfo
                     let m = movementinfo !! 1
                     let l = checkMovement lines k m
                     if l
                         then do putStrLn "Liigutan!\n"
                                 let n = move lines k m
                                 putStrLn(fancyPrint n)
                                 let c = checker n yGate
                                 play n yGate k m c
                         else do putStrLn "Sisesta oiged liigutamise andmed!!\n"
                                 play lines yGate a s c
    | otherwise = putStrLn("\nSinu voit!")

{-
Blokkide liigutamise suunav funktsioon, mis jaotab 
liigutamised horisontaalseks ja vertikaalseks ning suunab
töö edasi vastavatesse funktsioonidesse.
-}
move [] a s = []
move (x:xs) a s
    | s == 'l' = movex (x:xs) a s
    | s == 'r' = movex (x:xs) a s
    | otherwise = movey (x:xs) a s

{-
Funktsioon blokkide horisontaalseks liigutamiseks
-}
movex [] a s = []
movex (x:xs) a s
    | elem a x = moveNow x a s : xs
    | otherwise = x : movex xs a s
    where
      moveNow x a s
        | s == 'l' = moveNowLeft x a
        {-
		Paremale liigutamine on põhimõtteliselt tagurpidi
		vasakule liigutamine. Kasutatud reverse, et saaks
		vasakule liigutamise funktsiooni rakendada.
		-}
        | s == 'r' = reverse $ moveNowLeft (reverse x) a
        where
            {-
			Konkreetselt blokkide vasakule nihutamise funktsioon.
			-}
            moveNowLeft [x] _    = [x]
            moveNowLeft (x:y:xs) a
              | x == ' ' && y == a = a : moveNowLeft (x:xs) a
              | otherwise = x : moveNowLeft (y:xs) a

{-
Funktsioon blokkide vertikaalseks liigutamiseks.
Kasutatud on maatriksi transponeerimist, et saaks vertikaalse
liigutamise pealt (mis on sõnedega mõnevõrra ebamugavam)
horisontaalse peale üle minna. Üles liigutamine vastab 
transponeeritult vasakule liigutamisele ja alla liigutamine
paremale liigutamisele.
-}
movey [] a s = []
movey (x:xs) a s
    | s == 'u' = transpose $ movex (transpose (x:xs)) a 'l'
    | s == 'd' = transpose $ movex (transpose (x:xs)) a 'r'
    | otherwise = (x:xs)

{-
Funktsioon väljapääsu avaga rea numbri leidmiseks.
-}
gateFinderY [] = 0
gateFinderY (x:xs)
    | last x == ' ' = 0
    | otherwise = 1 + gateFinderY xs

{-
Funktsioon, mis kontrollib, kas mäng on jõudnud lõppseisundisse.
-}
checker [] _ = False
checker (x:xs) yGate
    | last ((init (x:xs)) !! yGate) == '0' = True
    | otherwise = False

{-
Funktsioon, mis käib läbi seisundite listi arvuti mängimise
juhul. Iga seisundi juures kasutatakse lõppseisundi kontrolli ja 
sellise leidmisel tagastatakse True ning põhifunktsioonis suunab
see programmi töö lõpetamisele
-}
checkerAI [] _ = False
checkerAI (x:xs) yGate
    | checker x yGate = True
    | otherwise = checkerAI xs yGate

{-
Funktsioon, mis tagastab kõikide mängus olevate 
blokkide numbrite listi.
-}
getBlocks [] = [] 
getBlocks (x:xs) 
    | 1 == 1 = drop 3 (keys(fromListWith (+) [(c, 0) | c <- fancyPrint(x:xs)]))

{-
Funktsioon kõigi seisundite listist korduvate elementide eemaldamiseks.
-}
removeDuplicates2 = List.foldl (\seen z -> if z `elem` seen
                                      then seen
                                      else seen ++ [z]) []

{-
Funktsioon, mida kasutatakse arvuti mängimise juhul blokkide liigutamise
kontrollimiseks. Kasutab eelnevalt loodud blokkide lubatud liigutuste listi.
-}
moveAI [] a s movements = []
moveAI (x:xs) a s movements
    | s == 'l' && lookupAI movements a == 'h' = movex (x:xs) a s
    | s == 'r' && lookupAI movements a == 'h' = movex (x:xs) a s
    | s == 'u' && lookupAI movements a == 'v' = movey (x:xs) a s
    | s == 'd' && lookupAI movements a == 'v' = movey (x:xs) a s
    | otherwise = []
{-
Funktsioon, mis loob kõik lubatud seisundid. 
Eelnevalt loodud tühjad seisundid  (ja korduvad seisundid) jätab vahele.
-}
generateStates [] _ _= []
generateStates (x:xs) blocks movements
    | x == [] = generateStates xs blocks movements
    {-Kuna korduvad seisundid eemaldatakse teise funktsiooniga, 
    siis see osa pole vajalik. Võit 1 sekund kui ei kasuta :)
    | elem x xs = generateStates xs blocks movements
	-}
    | otherwise = (steps x blocks movements) ++ generateStates xs blocks movements
        where 
            steps x [] movements = []
            steps x (y:ys) movements 
                | 1==1 =  moveAI x y 'l' movements : moveAI x y 'r' movements : moveAI x y 'u' movements : moveAI x y 'd' movements : steps x ys movements

{-Koos täispuu loomisega, et leida läbitud tee, hetkel ei kasutata,
välja kommenteeritud-}
{-
generateStates [] _ _= []
generateStates (x:xs) blocks movements
    | x == [] = addInvalidResults blocks ++ generateStates xs blocks movements
    | elem x xs = addInvalidResults blocks ++ generateStates xs blocks movements
    | otherwise = (steps x blocks movements) ++ generateStates xs blocks movements
        where 
            addInvalidResults [y] = [[],[],[],[]]
            addInvalidResults (y:ys) = [[],[],[],[]] ++ addInvalidResults ys
            steps x [] movements = []
            steps x (y:ys) movements 
                | 1==1 =  moveAI x y 'l' movements : moveAI x y 'r' movements : moveAI x y 'u' movements : moveAI x y 'd' movements : steps x ys movements
-}

{-
Funktsioon, mis kontrollib arvuti mängimist.
-}
playAI (x:xs) c blocks yGate movements finish {-kaasasOlevList-}
    {-
	Kui on True, et nulli ees on vaba tee väljapääsu auguni.
	Teeb kiirema lõpu funktsiooniga finishIt, ehk ei käi
	enam järgmist taset läbi, et ükshaaval 0 lõppu nihutada.
	-}
    | finish = do
                let vastus = findWhere (x:xs)
                putStrLn("Viimane seis oli : \n" ++ fancyPrint(vastus))

                {-Täispuust eelmiste elementide leidmise osa.
 				  Siin peaks jooksutama funktsiooni.

                    Midagi sellist äkki ???

                    showSteps i (x:xs) = 
                         showSteps1 i xs
                            where
                              showSteps1 0 _ = []
                              showSteps1 _ [] = []
                              showSteps1 i (x:xs) = (x !! (i `div` 8)) ++ showSteps1 (i `div` 8) xs

                    Probleem aga selles listis... Tundub väga kahtlane.
                    Hetkel prindib välja listi esimeste kolme liikme suurused

                    ########
                    #      #
                    #   2  #
                    #00 2 1 
                    #   2 1#
                    #      #
                    #      #
                    ########

                 -}
                {- Täispuuga eelmiste tee elementide leidmiseks
                print(length kaasasOlevList)
                print(length $ kaasasOlevList !! 0)
                print(length $ kaasasOlevList !! 1)
                print(length $ kaasasOlevList !! 2)
                let index = (List.elemIndex vastus (head kaasasOlevList))
                -}
                finishIt vastus
    {-
    Funktsiooni põhiosa, mis rakendub juhul kui ei ole lõpus
	ega pole nullbloki tee lõppu vaba.
    -}
    | c == False = do let c = checkerAI (x:xs) yGate
                      let l = removeDuplicates2 (List.filter (not . List.null) $ generateStates (x:xs) blocks movements)
                      {- Tee leidmisega variant
                      let l2 = generateStates (x:xs) blocks movements
                      let kaasasOlevList2 = l2 : kaasasOlevList
                      -}
                      let f = checkForFinish l
                      putStrLn("Laiuti tase läbitud")
                      playAI l c blocks yGate movements f {-kaasasOlevList2-}
    {-
	Rakendub juhul, kui mingil põhjusel esimene tingimus üldse ei rakendu.
	Seda tundub et ei tohiks juhtudagi.
	-}
    | otherwise = do 
                    let vastus = findWhere (x:xs)
                    putStrLn("Viimane seis oli : \n" ++ fancyPrint(vastus))
                    putStrLn("Lahendus leitud")
                    where
                      finishIt x
                        | (checker x yGate) = do
                            putStrLn("Loppseis:")
                            putStrLn(fancyPrint(x))
                        | otherwise = finishIt (movex x '0' 'r')
                      checkForFinish [] = False  
                      checkForFinish (x:xs)
                        | sprintChecker x = True
                        | otherwise = checkForFinish xs
                      findWhere (x:xs)
                        | sprintChecker x = x
                        | otherwise = findWhere xs

{-
Funktsioon, mis kontrollib, kas 0 on oma reas viimane
ehk kas nulli teekond lõppu on vaba ja seega mäng kiiremini
lõpetatav.
-}
sprintChecker [] = False                              
sprintChecker (x:xs)
    | elem '0' x = sC x
    | otherwise = sprintChecker xs
        where
         {- Rea elemendid filtreeritakse, et eemaldada
         tühikud ning korduvad elemendid 0 teekonna
         tühjuse saamiseks
		 -}
          sC a = sC2 $ nub(List.filter (/=' ') a)        
              where
                sC2 y
                  | last y ==  '0' = True
                  | otherwise = False

{-
Peafunktsioon, kus luuakse enne mängimist ka vajalikud muutujad.
-}
main :: IO ()
main = do
    { hSetBuffering stdin LineBuffering
    ; putStr "\nTeretulemast mängima mängu UnblockMe!\n"
    ; putStr "\nKui soovite ise mängu mängida, kirjuta M ja vajuta enter,\nkui tahad vaadata, kuidas arvuti mängib,\nkirjuta ükskoik mida muud ja vajuta enter. \n\n"
    ; content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; let yGate = gateFinderY lines
    ; let blocks = getBlocks lines
    ; let states = [lines]
    ; let movements = checkMovementAI blocks lines
    {-
    ; print(movements)
	-}
    ; playinfo <- getLine
    ; putStrLn("Algseis: ")
    ; putStrLn(fancyPrint(lines))
    ; if head playinfo == 'M' then play lines yGate ' ' ' ' False
      else playAI states False blocks yGate movements False {-[]-}
    }
