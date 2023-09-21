module Lists where

  -- Write a function to build an (unbounded) list of counting numbers (counting numbers start at 1). The counting numbers are the set {1, 2, 3, ...}. You may assume n is a positive integer.
  countingNumbers :: [Int]
  countingNumbers = [1..]

  -- Write a function to build a list of the multiples of the passed argument.
  multiplesOfNumbers :: Int -> [Int]
  multiplesOfNumbers n = [n * x | x <- countingNumbers]

  -- Write a function to build a list of the "Woodall numbers"
  woodallNumbers :: [Int]
  woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

  -- Write a function to build a list of the "Padovan sequence of numbers"
  padovanNumbers :: [Int]
  padovanNumbers = [aux n | n <- [0..]]
    where
      aux 0 = 1
      aux 1 = 1
      aux 2 = 1
      aux n = aux (n - 2) + aux (n - 3)

  -- Write a function to merge (in sorted order) two lists of numbers (assuming both lists are already sorted).
  -- Using the passed binary comparator function which chooses the element from the first list if true and from the second list if false.
  order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  order f [] ys = ys
  order f xs [] = xs
  order f (x:xs) (y:ys)
    | f x y = x : order f xs (y:ys)
    | otherwise = y : order f (x:xs) ys

  -- Write a function to build a list of lists where each sub-list is a pair of elements (in sequence) in the original list.
  -- For example the nth sub-list is the elements from 2*i - 1 and 2*i in the list or just 2*i - 1 if there is an odd number of elements in the list.
  pairUp :: [a] -> [[a]]
  pairUp [] = []
  pairUp [x] = [[x]]
  pairUp (x:y:ys) = [x, y] : pairUp ys

  -- Write a function to build the run-length encoding of a list.
  -- The run-length encoding is a list of tuples where each tuple is a pair consisting of the number of elements found consecutively in the list.
  -- The examples below demonstrate how it should work.
  runLengthEncoding :: (Eq a) => [a] -> [(a, Int)]
  runLengthEncoding [] = []
  runLengthEncoding (x:xs) = aux 1 x xs
    where
      aux n x  [] = [(x, n)]
      aux n x  (y:ys)
        | x == y = aux (n + 1) x ys
        | otherwise = (x, n) : aux 1 y ys

  -- Write a function to apply a binary function f chosen in order from a list of functions fs to the pair of elements (or possibly a singleton element) in the list xs produced by pairUp.
  -- If there are fewer functions in the list of functions, then the list of functions should wrap, starting over as needed. You should assume that every sublist has at least one element.
  -- The result of applying a function to a one element sublist is the value of that element.
  listPairApply :: (Num a) => [(a -> a -> a)] -> [[a]] -> [a]
  listPairApply [] [] = []
  listPairApply fs [] = []
  listPairApply (f:fs) (x:xs)
   | even (length x) = f (x !! 0) (x !! 1) : listPairApply (fs ++ [f]) xs
   | otherwise = x !! 0 : listPairApply fs xs

  -- Write a function to build a function that is the composition of the functions in the list xs.
  -- You may assume each function in the list is a unary function (takes one argument).
  composeList :: [(a -> a)] -> (a -> a)
  composeList [] = \x -> x
  composeList (f:fs) = foldr (.) f fs
