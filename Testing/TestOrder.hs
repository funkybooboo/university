order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
order f xs ys = [if (x `f` y) then x else y | (x, y) <- zip xs ys]

woodallNumbers :: [Int]
woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

multiplesOfNumbers :: Int -> [Int]
multiplesOfNumbers n = [n * x | x <- countingNumbers]

main :: IO ()
main = do

  putStrLn "Testing order with: order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)) -> [1, 2, 4, 6, 7, 8, 10, 23, 63, 159]"
  print (order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)))
