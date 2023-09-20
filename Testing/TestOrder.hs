order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
order f [] ys = ys
order f xs [] = xs
order f (x:xs) (y:ys)
  | f x y = x : order f xs (y:ys)
  | otherwise = y : order f (x:xs) ys

countingNumbers :: [Int]
countingNumbers = [1..]

woodallNumbers :: [Int]
woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

multiplesOfNumbers :: Int -> [Int]
multiplesOfNumbers n = [n * x | x <- countingNumbers]

main :: IO ()
main = do

  putStrLn "Testing order with: order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)) -> [1, 2, 4, 6, 7, 8, 10, 23, 63, 159]"
  print (order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)))
