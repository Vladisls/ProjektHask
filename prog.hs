import Data.List.Split

fancyPrint [] = ""
fancyPrint (x:xs) =  x ++ " " ++ (fancyPrint xs)

count [] _ = 0
count (x:xs) v
  | elem v x = 1 + count xs v
  | otherwise = 0 + count xs v

movex [] a s = []
movex (x:xs) a s
    | elem a x = moveNow x a s
    | otherwise = x : (movex xs a s)
    where
      moveNow x a s
        | s == 'l' = moveNow2 x a
        where
            moveNow2 [] _ = []
            moveNow2 (x:y:xs) a
              | x == ' ' && y == a = a : x : moveNow2 (y:xs) a
              | otherwise = x : moveNow2 (y:xs) a

main :: IO ()
main = do
    { content <- readFile "laud.txt"
    ; let lines = splitOn "\n" content
    ; print (fancyPrint lines)
    ; print (count lines '1')
    }
