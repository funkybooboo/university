padovanNumbers :: [Integer]
padovanNumbers = [aux n | n <- [0..]]
  where
    aux 0 = 1
    aux 1 = 1
    aux 2 = 1
    aux n = aux (n - 2) + aux (n - 3)

main :: IO ()
main = do

  putStrLn "Testing padovanNumbers with: take 2 padovanNumbers -> [1, 1]"
  print (take 2 padovanNumbers)

  putStrLn "\nTesting padovanNumbers with: take 10 padovanNumbers -> [1, 1, 1, 2, 2, 3, 4, 5, 7, 9]"
  print (take 10 padovanNumbers)
