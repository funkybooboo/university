countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing countingNumbers with: take 3 countingNumbers -> [1, 2, 3]"
  print (take 3 countingNumbers)

  putStrLn "\nTesting countingNumbers with: take 5 countingNumbers -> [1, 2, 3, 4, 5]"
  print (take 5 countingNumbers)