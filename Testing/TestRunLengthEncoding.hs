runLengthEncoding :: a -> b
runLengthEncoding [] = []
runLengthEncoding xs = aux (head xs) 1 (tail xs)
  where
    aux i count [] = [(i, count)]
    aux i count (j:js)
      | i == j = aux i (count + 1) js
      | otherwise = (i, count) : aux j 1 js

main :: IO ()
main = do
    let list1 = []
    let list2 = [7]
    let list3 = [7, 7, 4, 7, 7, 7]

    putStrLn "Testing runLengthEncoding with empty list:"
    print (runLengthEncoding list1)

    putStrLn "\nTesting runLengthEncoding with [7]:"
    print (runLengthEncoding list2)

    putStrLn "\nTesting runLengthEncoding with [7, 7, 4, 7, 7, 7]:"
    print (runLengthEncoding list3)
