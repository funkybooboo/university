runLengthEncoding :: (Eq a) => [a] -> [(a, Int)]
runLengthEncoding [] = []
runLengthEncoding (x:xs) = aux 1 x xs
  where
    aux n x  [] = [(x, n)]
    aux n x  (y:ys)
      | x == y = aux (n + 1) x ys
      | otherwise = (x, n) : aux 1 y ys

padovanNumbers :: [Integer]
padovanNumbers = [aux n | n <- [0..]]
  where
    aux 0 = 1
    aux 1 = 1
    aux 2 = 1
    aux n = aux (n - 2) + aux (n - 3)

main :: IO ()
main = do

  putStrLn "Testing runLengthEncoding with: runLengthEncoding [] -> []"
  print (runLengthEncoding ([] :: [Int]))

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding [7] -> [(7,1)]"
  print (runLengthEncoding [7])

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding [7, 7, 4, 7, 7, 7] -> [(7,2), (4, 1), (7, 3)]"
  print (runLengthEncoding [7, 7, 4, 7, 7, 7])

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding (take 10 padovanNumbers) -> [(1,3) (2,2), (3,1), (4,1), (5,1), (7,1), (9,1)]"
  print (runLengthEncoding (take 10 padovanNumbers))