listPairApply :: (Num a) => [[a -> a -> a]] -> [[a]] -> [[a]]
listPairApply [] [] = []
listPairApply fs [] = []
listPairApply [] xs = []
listPairApply fs xs = aux fs (head fs) (tail fs) (head xs) (tail xs)
  where
    aux ofs f fs x []
      | even (length x) = [[f (x !! 0) (x !! 1)]]
      | otherwise = x
    aux ofs f [] x xs
      | even (length x) = [f (x !! 0) (x !! 1)] : aux ofs (head ofs) (tail ofs) (head xs) (tail xs)
      | otherwise = x : aux ofs (head ofs) (tail ofs) (head xs) (tail xs)
    aux ofs f fs x xs
      | even (length x) = [f (x !! 0) (x !! 1)] : aux ofs (head fs) (tail fs) (head xs) (tail xs)
      | otherwise = x : aux ofs (head fs) (tail fs) (head xs) (tail xs)

countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing listPairApply with: listPairApply [(+),(*),(*),(*)] [] -> []"
  print (listPairApply [(+),(*),(*),(*)] [])

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)) -> [3, 12, 30]"
  print (listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)))

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)) -> [3, 12, 11, 56]"
  print (listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)))