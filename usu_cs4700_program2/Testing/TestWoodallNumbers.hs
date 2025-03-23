woodallNumbers :: [Int]
woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

countingNumbers :: [Int]
countingNumbers = [1..]

main :: IO ()
main = do

  putStrLn "Testing woodallNumbers with: take 1 woodallNumbers -> [1]"
  print (take 1 woodallNumbers)

  putStrLn "\nTesting woodallNumbers with: take 5 woodallNumbers -> [1, 7, 23, 63, 159]"
  print (take 5 woodallNumbers)