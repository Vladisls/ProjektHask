import Data.List.Split
import Data.List

fancyPrint [] = ""
fancyPrint (x:xs) =  (x ++ "\n") ++ (fancyPrint xs)

count [] _ = 0
count (x:xs) v
  | elem v x = 1 + count xs v
  | otherwise = 0 + count xs v

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

gateFinderX (x:xs) = (length x) - 1
gateFinderY [] = NULL
gateFinderY (x:xs)
    | x[-1] == ' ' = 0
    | otherwise = 1 + gateFinderY

checker (x:xs)
  | (x:xs)[yGate][xGate] == '1' = True
  | otherwise = False
main :: IO ()
main = do
    { content <- readFile "laud.txt"
    ; let xGate = gateFinderX
    ; let yGate = gateFinderY
    ; let lines = splitOn "\n" content
    ; putStrLn (fancyPrint lines)
    ; let lines2 = movex lines '1' 'l'
    ; putStrLn (fancyPrint lines2)
    ; let lines3 = movex lines2 '3' 'r'
    ; putStrLn (fancyPrint lines3)
    ; print(count lines '1')
    }
