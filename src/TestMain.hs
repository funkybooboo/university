import Lists

add1 :: Int -> Int
add1 x = 1 + x

add2 :: Int -> Int
add2 x = 2 + x

add3 :: Int -> Int
add3 x = 3 + x

main :: IO ()
main = do

  putStrLn "Testing countingNumbers with: take 3 countingNumbers -> [1, 2, 3]"
  print (take 3 countingNumbers)

  putStrLn "\nTesting countingNumbers with: take 5 countingNumbers -> [1, 2, 3, 4, 5]"
  print (take 5 countingNumbers)

  putStrLn "\n----------------------------------------\n"

  putStrLn "\nTesting multiplesOfNumbers with: take 1 (multiplesOfNumbers 5) -> [5]"
  print (take 1 (multiplesOfNumbers 5))

  putStrLn "\nTesting multiplesOfNumbers with: take 5 (multiplesOfNumbers 2) -> [2, 4, 6, 8, 10]"
  print (take 5 (multiplesOfNumbers 2))

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing woodallNumbers with: take 1 woodallNumbers -> [1]"
  print (take 1 woodallNumbers)

  putStrLn "\nTesting woodallNumbers with: take 5 woodallNumbers -> [1, 7, 23, 63, 159]"
  print (take 5 woodallNumbers)

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing padovanNumbers with: take 2 padovanNumbers -> [1, 1]"
  print (take 2 padovanNumbers)

  putStrLn "\nTesting padovanNumbers with: take 10 padovanNumbers -> [1, 1, 1, 2, 2, 3, 4, 5, 7, 9]"
  print (take 10 padovanNumbers)

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing order with: order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)) -> [1, 2, 4, 6, 7, 8, 10, 23, 63, 159]"
  print (order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)))

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing pairUp with: pairUp [] -> []"
  print (pairUp ([] :: [Int]))

  putStrLn "Testing pairUp with: pairUp (take 3 countingNumbers) -> [[1, 2], [3]]"
  print (pairUp (take 3 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 5 countingNumbers) -> [[1, 2], [3, 4], [5]]"
  print (pairUp (take 5 countingNumbers))

  putStrLn "\nTesting pairUp with: pairUp (take 6 countingNumbers) -> [[1, 2], [3, 4], [5, 6]]"
  print (pairUp (take 6 countingNumbers))

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing runLengthEncoding with: runLengthEncoding [] -> []"
  print (runLengthEncoding ([] :: [Int]))

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding [7] -> [(7,1)]"
  print (runLengthEncoding [7])

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding [7, 7, 4, 7, 7, 7] -> [(7,2), (4, 1), (7, 3)]"
  print (runLengthEncoding [7, 7, 4, 7, 7, 7])

  putStrLn "\nTesting runLengthEncoding with: runLengthEncoding (take 10 padovanNumbers) -> [(1,3) (2,2), (3,1), (4,1), (5,1), (7,1), (9,1)]"
  print (runLengthEncoding (take 10 padovanNumbers))

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing listPairApply with: listPairApply [(+),(*),(*),(*)] [] -> []"
  print (listPairApply [(+),(*),(*),(*)] [])

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)) -> [3, 12, 30]"
  print (listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)))

  putStrLn "\nTesting listPairApply with: listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)) -> [3, 12, 11, 56]"
  print (listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)))

  putStrLn "\n----------------------------------------\n"

  putStrLn "Testing composeList with: composeList [add1, add2, add3] 3 -> 9"
  print (composeList [add1, add2, add3] 3)

  putStrLn "Testing composeList with: composeList [] 3 -> 3"
  print (composeList [] 3)