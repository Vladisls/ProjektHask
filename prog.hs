import Data.List.Split
import Data.List
import System.IO

fancyPrint [] = ""
fancyPrint (x:xs) =  (x ++ "\n") ++ (fancyPrint xs)

count [] _ = 0
count (x:xs) v
  | elem v x = 1 + count xs v
  | otherwise = 0 + count xs v

  

play lines yGate a s
    | checker lines yGate==False = do hSetBuffering stdin NoBuffering
                                      putStrLn(fancyPrint lines)
                                      putStr "tere1\n"
                                      movementinfo <- getLine
                                      putStrLn "tere2"
                                      play (move lines a s) yGate (head movementinfo) (movementinfo !! 1)
    | otherwise = putStrLn(fancyPrint lines ++ "tere3")


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

main :: IO ()
main = do
    { content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; let yGate = gateFinderY lines
    ; play lines yGate ' ' ' '
    ; let lines2 = movex lines '1' 'l'
    ; putStrLn (fancyPrint lines2)
    ; let lines3 = movex lines2 '3' 'r'
    ; putStrLn (fancyPrint lines3)
    }
