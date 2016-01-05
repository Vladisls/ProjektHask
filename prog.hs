{-# LANGUAGE FlexibleContexts #-}

import Data.List.Split
import Data.List as List
import Data.Map as Map
import qualified Data.Set as Set
import System.IO
import Debug.Trace


merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

fancyPrint [] = ""
fancyPrint (x:xs) =  (x ++ "\n") ++ (fancyPrint xs)

count [] _ = 0
count (x:xs) v
  | elem v x = 1 + count xs v
  | otherwise = 0 + count xs v

checkMovement lines a s
  | (count lines a)>1 && s == 'u' = True
  | (count lines a)>1 && s == 'd' = True
  | (count lines a)==1 && s == 'l' = True
  | (count lines a)==1 && s == 'r' = True
  | otherwise = False

checkMovementAI [] lines = []
checkMovementAI (x:xs) lines
  | (count lines x)>1 = ([x] ++ ['v']) : checkMovementAI xs lines
  | (count lines x)==1 = ([x] ++ ['h']) : checkMovementAI xs lines
  | otherwise = ([x] ++ ['o']) : checkMovementAI xs lines

lookupAI [] a = 'o'
lookupAI (x:xs) a
  | x !! 0 == a = x !! 1
  | otherwise = lookupAI xs a

play lines yGate a s c
    | c == False =do hSetBuffering stdin LineBuffering
                     putStr "Hetkeseis on selline: \n"
                     putStrLn(fancyPrint lines)
                     putStr "Palun liigutage klotse käskudega (1r || 2r || (nr)(suund) )\n"
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
    | otherwise = putStrLn((fancyPrint lines) ++ "\nSinu voit!")

move [] a s = []
move (x:xs) a s
    | s == 'l' = movex (x:xs) a s
    | s == 'r' = movex (x:xs) a s
    | otherwise = movey (x:xs) a s

movex [] a s = []
movex (x:xs) a s
    | elem a x = moveNow x a s : xs
    | otherwise = x : movex xs a s
    where
      moveNow x a s
        | s == 'l' = moveNowLeft x a
        | s == 'r' = reverse $ moveNowLeft (reverse x) a
        where
            moveNowLeft [x] _    = [x]
            moveNowLeft (x:y:xs) a
              | x == ' ' && y == a = a : moveNowLeft (x:xs) a
              | otherwise = x : moveNowLeft (y:xs) a

movey [] a s = []
movey (x:xs) a s
    | s == 'u' = transpose $ movex (transpose (x:xs)) a 'l'
    | s == 'd' = transpose $ movex (transpose (x:xs)) a 'r'
    | otherwise = (x:xs)

gateFinderY [] = 0
gateFinderY (x:xs)
    | last x == ' ' = 0
    | otherwise = 1 + gateFinderY xs

checker [] _ = False
checker (x:xs) yGate
    | last ((init (x:xs)) !! yGate) == '0' = True
    | otherwise = False

checkerAI [] _ = False
checkerAI (x:xs) yGate
    | checker x yGate = True
    | otherwise = checkerAI xs yGate


getBlocks [] = [] 
getBlocks (x:xs) 
    | 1 == 1 = drop 3 (keys(fromListWith (+) [(c, 0) | c <- fancyPrint(x:xs)]))

removeDuplicates2 = List.foldl (\seen z -> if z `elem` seen
                                      then seen
                                      else seen ++ [z]) []


moveAI [] a s movements = []
moveAI (x:xs) a s movements
    | s == 'l' && lookupAI movements a == 'h' = movex (x:xs) a s
    | s == 'r' && lookupAI movements a == 'h' = movex (x:xs) a s
    | s == 'u' && lookupAI movements a == 'v' = movey (x:xs) a s
    | s == 'd' && lookupAI movements a == 'v' = movey (x:xs) a s
    | otherwise = []

generateStates [] _ _= []
generateStates (x:xs) blocks movements
    | x == [] = generateStates xs blocks movements
    | elem x xs = generateStates xs blocks movements
    | otherwise = (steps x blocks movements) ++ generateStates xs blocks movements
        where 
            steps x [] movements = []
            steps x (y:ys) movements 
                | 1==1 =  moveAI x y 'l' movements : moveAI x y 'r' movements : moveAI x y 'u' movements : moveAI x y 'd' movements : steps x ys movements

fancyPrint2 [] = ""
fancyPrint2 (x:xs) = (fancyPrint x ++ "\n") ++ fancyPrint2 xs

playAI (x:xs) c blocks yGate movements finish kaasasOlevList
    | finish = do
                let vastus = findWhere (x:xs)
                putStrLn("Viimane seis oli : \n" ++ fancyPrint(vastus))
                let eelnevVastus = elemIndex vastus kaasasOlevList
                print(eelnevVastus)
                print(1747 `div` 2)
                putStrLn(fancyPrint(kaasasOlevList !! 1931))
                putStrLn(fancyPrint(kaasasOlevList !! 483))
                print(length kaasasOlevList)
                finishIt vastus
    | c == False = do let c = checkerAI (x:xs) yGate
                      let l = removeDuplicates2 (List.filter (not . List.null) $ generateStates (x:xs) blocks movements)
                      let l2 = generateStates (x:xs) blocks movements
                      let kaasasOlevList2 = kaasasOlevList ++ l2
                      let f = checkForFinish l
                      putStrLn("Laiuti tase läbitud")
                      playAI l c blocks yGate movements f kaasasOlevList2
    | otherwise = do 
                    let vastus = findWhere (x:xs)
                    putStrLn("Viimane seis oli : \n" ++ fancyPrint(vastus))
                    putStrLn("Lahendus leitud")
                    where
                      finishIt x
                        | (checker x yGate) = do
                            putStrLn("Jooksen lopuni")
                        | otherwise = finishIt (movex x '0' 'r')
                      checkForFinish [] = False  
                      checkForFinish (x:xs)
                        | sprintChecker x = True
                        | otherwise = checkForFinish xs
                      findWhere (x:xs)
                        | sprintChecker x = x
                        | otherwise = findWhere xs
sprintChecker (x:xs)
    | elem '0' x = sC x
    | otherwise = sprintChecker xs
        where
          sC a = sC2 $ nub(List.filter (/=' ') a)        
              where
                sC2 y
                  | last y ==  '0' = True
                  | otherwise = False

main :: IO ()
main = do
    { hSetBuffering stdin LineBuffering
    ; putStr "Teretulemast mängima mängu UnblockMe!\n"
    ; putStr "Kui soovite ise mängu mängida, kirjuta M ja vajuta enter,\n kui tahad vaadata, kuidas arvuti mängib,\n kirjuta ükskoik mida muud ja vajuta enter. \n"
    ; content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; let yGate = gateFinderY lines
    ; let blocks = getBlocks lines
    ; let states = [lines]
    ; let movements = checkMovementAI blocks lines
    ; print(movements)
    ; playinfo <- getLine
    ; if head playinfo == 'M' then play lines yGate ' ' ' ' False
      else playAI states False blocks yGate movements False []
    }
