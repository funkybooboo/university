pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp (x:y:ys) = [x, y] : pairUp ys

countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing pairUp with: pairUp [] -> []"
  print (pairUp ([] :: [Int]))

  putStrLn "Testing pairUp with: pairUp (take 3 countingNumbers) -> [[1, 2], [3]]"
  print (pairUp (take 3 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 5 countingNumbers) -> [[1, 2], [3, 4], [5]]"
  print (pairUp (take 5 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 6 countingNumbers) -> [[1, 2], [3, 4], [5, 6]]"
  print (pairUp (take 6 countingNumbers))
