pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [x] = [[x]]
pairUp xs | even (length xs) = [[x, y] | (x, y) <- zip h1 h2]
          | otherwise = pairUp (init xs) ++ [[last xs]]
  where
    (h1, h2) = splitAt (length xs `div` 2) xs

main :: IO ()
main = do
    let testList1 = [1, 2, 3, 4, 5, 6]
    let testList2 = [1, 2, 3, 4, 5]

    putStrLn "Testing pairUp with even-length list:"
    print (pairUp testList1)

    putStrLn "\nTesting pairUp with odd-length list:"
    print (pairUp testList2)
