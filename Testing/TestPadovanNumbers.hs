padovanNumbers :: [Integer]
padovanNumbers = [aux n | n <- [0..]]
  where
    aux 0 = 1
    aux 1 = 1
    aux 2 = 1
    aux n = aux (n - 2) + aux (n - 3)

main :: IO ()
main = do

    putStrLn "Testing padovanNumbers with taking 2:"
    print (take 2 padovanNumbers)

    putStrLn "\nTesting padovanNumbers with taking 10:"
    print (take 10 padovanNumbers)
