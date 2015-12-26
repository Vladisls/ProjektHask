import Data.List.Split

fancyPrint [] = ""
fancyPrint (x:xs) =  x ++ " " ++ (fancyPrint xs)

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

main :: IO ()
main = do
    { content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; print (fancyPrint lines)
    ; print (fancyPrint $ movex lines '1' 'l')
    ; print (fancyPrint $ movex lines '1' 'r')
    ; print (fancyPrint $ movex lines '3' 'r')
    ; print (fancyPrint $ movex lines '3' 'l')
    ; print (fancyPrint lines)
    ; print (count lines '1')
    }
