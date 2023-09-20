module Lists where

  -- Write a function to build an (unbounded) list of counting numbers (counting numbers start at 1). The counting numbers are the set {1, 2, 3, ...}. You may assume n is a positive integer.
  countingNumbers :: [Integer]
  countingNumbers = [1..]

  -- Write a function to build a list of the multiples of the passed argument.
  multiplesOfNumbers :: Integer -> [Integer]
  multiplesOfNumbers n = [n * x | x <- countingNumbers]

  -- Write a function to build a list of the "Woodall numbers"
  woodallNumbers :: [Integer]
  woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

  -- Write a function to build a list of the "Padovan sequence of numbers"
  aux :: Integer -> Integer
  aux 0 = 1
  aux 1 = 1
  aux 2 = 1
  aux n = aux (n - 2) + aux (n - 3)
  padovanNumbers :: [Integer]
  padovanNumbers = [aux n | n <- [0..]]

  -- Write a function to merge (in sorted order) two lists of numbers (assuming both lists are already sorted).
  -- Using the passed binary comparator function which chooses the element from the first list if true and from the second list if false.
  order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  order f xs ys = [if (x `f` y) then x else y | (x, y) <- zip xs ys]

  -- Write a function to build a list of lists where each sub-list is a pair of elements (in sequence) in the original list.
  -- For example the nth sub-list is the elements from 2*i - 1 and 2*i in the list or just 2*i - 1 if there is an odd number of elements in the list.
  pairUp :: [a] -> [[a]]
  pairUp [] = []
  pairUp [x] = [[x]]
  pairUp xs | even (length xs) = [[x, y] | (x, y) <- zip h1 h2]
            | otherwise = [[last xs]] ++ pairUp (init xs)
    where
      (h1, h2) = splitAt (length xs `div` 2) xs

  -- Write a function to build the run-length encoding of a list.
  -- The run-length encoding is a list of tuples where each tuple is a pair consisting of the number of elements found consecutively in the list.
  -- The examples below demonstrate how it should work.
  runLengthEncoding :: [a] -> [(a)]
  runLengthEncoding [] = []
  runLengthEncoding [x] = [(x, 1)]
  runLengthEncoding xs =