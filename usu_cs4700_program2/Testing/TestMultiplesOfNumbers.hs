multiplesOfNumbers :: Int -> [Int]
multiplesOfNumbers n = [n * x | x <- countingNumbers]

countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing multiplesOfNumbers with: take 1 (multiplesOfNumbers 5) -> [5]"
  print (take 1 (multiplesOfNumbers 5))

  putStrLn "\nTesting multiplesOfNumbers with: take 5 (multiplesOfNumbers 2) -> [2, 4, 6, 8, 10]"
  print (take 5 (multiplesOfNumbers 2))