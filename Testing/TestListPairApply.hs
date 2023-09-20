listPairApply :: (Num a) => [(a -> a -> a)] -> [[a]] -> [a]
listPairApply [] [] = []
listPairApply fs [] = []
listPairApply [] xs = []
listPairApply fs xs = aux fs (head fs) (tail fs) (head xs) (tail xs)
  where
    aux ofs f fs x []
      | even (length x) = (x !! 0) `f` (x !! 1)
      | otherwise = x !! 0
    aux ofs f [] x xs
      | even (length x) = (x !! 0) `f` (x !! 1) : aux ofs (head ofs) (tail ofs) (head xs) (tail xs)
      | otherwise = x !! 0 : aux ofs (head ofs) (tail ofs) (head xs) (tail xs)
    aux ofs f fs x xs
      | even (length x) = (x !! 0) `f` (x !! 1) : aux ofs (head fs) (tail fs) (head xs) (tail xs)
      | otherwise = x !! 0 : aux ofs (head fs) (tail fs) (head xs) (tail xs)

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