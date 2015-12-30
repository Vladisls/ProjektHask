import Data.List.Split
import Data.List as List
import Data.Map as Map
import Data.Set as Set
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

play lines yGate a s
    | checker lines yGate==False = do hSetBuffering stdin NoBuffering
                                      putStr "Hetkeseis on selline: \n"
                                      putStrLn(fancyPrint lines)
                                      putStr "Palun liigutage klotse käskudega (1r || 2r || (nr)(suund) )\n"
                                      movementinfo <- getLine
                                      putStr "Sisse loetud!\n"
                                      putStrLn "Liigutan!\n"
                                      let n = move lines a s
                                      putStrLn(fancyPrint n)
                                      play n yGate (head movementinfo) (movementinfo !! 1)
    | otherwise = putStrLn((fancyPrint lines) ++ "tere3")

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

checker (x:xs) yGate
    | last (x:xs) !! yGate == '0' = True
    | otherwise = False

getBlocks [] = [] 
getBlocks (x:xs) 
    | 1 == 1 = drop 4 (keys(fromListWith (+) [(c, 1) | c <- fancyPrint(x:xs)]))

generateStates [] _ = []
generateStates (x:xs) blocks
    | 1==1 = x : steps x blocks
                      where 
                        steps x [] = []
                        steps x (y:ys) 
                          | 1==1 =  move x y 'l' : move x y 'r' : move x y 'u' : move x y 'd' : steps x ys

fancyPrint2 [] = ""
fancyPrint2 (x:xs) = (fancyPrint x) ++ fancyPrint2 xs


main :: IO ()
main = do
    { content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; let yGate = gateFinderY lines
    ; print (getBlocks lines)
    ; let blocks = getBlocks lines
    ; let states = [lines,lines]
    ; print blocks
    ; print states
    ; print (generateStates states blocks)
    ; putStrLn(fancyPrint2 (generateStates states blocks) )
    }
