listPairApply :: (Num a) => [(a -> a -> a)] -> [[a]] -> [a]
listPairApply [] [] = []
listPairApply fs [] = []
listPairApply (f:fs) (x:xs)
  | even (length x) = f (x !! 0) (x !! 1) : listPairApply (fs ++ [f]) xs
  | otherwise = x !! 0 : listPairApply fs xs

countingNumbers :: [Int]
countingNumbers = [1..]

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp (x:y:ys) = [x, y] : pairUp ys

main :: IO ()
main = do

  putStrLn "Testing listPairApply with: listPairApply [(+),(*),(*),(*)] [] -> []"
  print (listPairApply [(+),(*),(*),(*)] [])

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)) -> [3, 12, 30]"
  print (listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)))

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)) -> [3, 12, 11, 56]"
  print (listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)))