pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp xs | even (length xs) = [[x, y] | (x, y) <- zip h1 h2]
          | otherwise = pairUp (init xs) ++ [[last xs]]
  where
    (h1, h2) = splitAt (length xs `div` 2) xs

countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing pairUp with: pairUp [] -> []"
  print (pairUp [])

  putStrLn "Testing pairUp with: pairUp (take 3 countingNumbers) -> [[1, 2], [3]]"
  print (pairUp (take 3 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 5 countingNumbers) -> [[1, 2], [3, 4], [5]]"
  print (pairUp (take 5 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 6 countingNumbers) -> [[1, 2], [3, 4], [5, 6]]"
  print (pairUp (take 6 countingNumbers))
