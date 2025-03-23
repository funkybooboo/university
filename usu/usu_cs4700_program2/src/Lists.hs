module Lists where
  -- Function description and example input and output are from the assignment page.
  -- example input and output: in -> out

  -- Write a function to build an (unbounded) list of counting numbers (counting numbers start at 1). The counting numbers are the set {1, 2, 3, ...}. You may assume n is a positive integer.
  -- take 3 countingNumbers -> [1, 2, 3]
  -- take 5 countingNumbers -> [1, 2, 3, 4, 5]
  countingNumbers :: [Int]
  countingNumbers = [1..]

  -- Write a function to build a list of the multiples of the passed argument.
  -- take 1 (multiplesOfNumbers 5) -> [5]
  -- take 5 (multiplesOfNumbers 2) -> [2, 4, 6, 8, 10]
  multiplesOfNumbers :: Int -> [Int]
  multiplesOfNumbers n = [n * x | x <- countingNumbers]

  -- Write a function to build a list of the Woodall numbers.
  -- take 1 woodallNumbers -> [1]
  -- take 5 woodallNumbers -> [1, 7, 23, 63, 159]
  woodallNumbers :: [Int]
  woodallNumbers = [((n * (2 ^ n)) - 1) | n <- countingNumbers]

  -- Write a function to build a list of the Padovan sequence of numbers.
  -- take 2 padovanNumbers -> [1, 1]
  -- take 10 padovanNumbers -> [1, 1, 1, 2, 2, 3, 4, 5, 7, 9]
  padovanNumbers :: [Int]
  padovanNumbers = [aux n | n <- [0..]]
    where
      aux 0 = 1
      aux 1 = 1
      aux 2 = 1
      aux n = aux (n - 2) + aux (n - 3)

  -- Write a function to merge (in sorted order) two lists of numbers (assuming both lists are already sorted) using the passed binary comparator function which chooses the element from the first list if true and from the second list if false.
  -- order (<) (take 5 woodallNumbers) (take 5 (multiplesOfNumbers 2)) -> [1, 2, 4, 6, 7, 8, 10, 23, 63, 159]
  order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  order f [] ys = ys
  order f xs [] = xs
  order f (x:xs) (y:ys)
    | f x y = x : order f xs (y : ys)
    | otherwise = y : order f (x : xs) ys

  -- Write a function to build a list of lists where each sub-list is a pair of elements (in sequence) in the original list. For example the nth sub-list is the elements from 2*i - 1 and 2*i in the list or just 2*i - 1 if there is an odd number of elements in the list.
  -- pairUp [] -> []
  -- pairUp (take 3 countingNumbers) -> [[1, 2], [3]]
  -- pairUp (take 5 countingNumbers) -> [[1, 2], [3, 4], [5]]
  pairUp :: [a] -> [[a]]
  pairUp [] = []
  pairUp [x] = [[x]]
  pairUp (x:y:ys) = [x, y] : pairUp ys

  -- Write a function to build the run-length encoding of a list. The run-length encoding is a list of tuples where each tuple is a pair consisting of the number of elements found consecutively in the list. The examples below demonstrate how it should work.
  -- runLengthEncoding [] -> []
  -- runLengthEncoding [7] -> [(7,1)]
  -- runLengthEncoding [7, 7, 4, 7, 7, 7] -> [(7,2), (4, 1), (7, 3)]
  -- runLengthEncoding (take 10 padovanNumbers) -> [(1,3) (2,2), (3,1), (4,1), (5,1), (7,1), (9,1)]
  runLengthEncoding :: (Eq a) => [a] -> [(a, Int)]
  runLengthEncoding [] = []
  runLengthEncoding (x:xs) = aux 1 x xs
    where
      aux n x  [] = [(x, n)]
      aux n x  (y:ys)
        | x == y = aux (n + 1) x ys
        | otherwise = (x, n) : aux 1 y ys

  -- Write a function to apply a binary function f chosen in order from a list of functions fs to the pair of elements (or possibly a singleton element) in the list xs produced by pairUp. If there are fewer functions in the list of functions, then the list of functions should wrap, starting over as needed.
  -- You should assume that every sublist has at least one element. The result of applying a function to a one element sublist is the value of that element.
  -- Example: listPairApply [f1,f2] [[x1, x2], [y1, y2], [w1]] = [f1(x1, x2), f2(y1, y2), f1(w1)]
  -- listPairApply [(+),(*),(*),(*)] [] -> []
  -- listPairApply [(+),(*),(*)] (pairUp (take 6 countingNumbers)) -> [3, 12, 30]
  -- listPairApply [(+),(*)] (pairUp (take 8 countingNumbers)) -> [3, 12, 11, 56]
  listPairApply :: (Num a) => [(a -> a -> a)] -> [[a]] -> [a]
  listPairApply [] [] = []
  listPairApply fs [] = []
  listPairApply (f:fs) (x:xs)
    | even (length x) = f (x !! 0) (x !! 1) : listPairApply (fs ++ [f]) xs
    | otherwise = x !! 0 : listPairApply fs xs

  -- Write a function to build a function that is the composition of the functions in the list xs. You may assume each function in the list is a unary function (takes one argument).
  -- add1 x = 1 + x
  -- add2 x = 2 + x
  -- add3 x = 3 + x
  -- print ((composeList [add1,add2,add3]) 3) -> 9
  -- print ((composeList []) 3) -> 3
  composeList :: [(a -> a)] -> (a -> a)
  composeList [] = \x -> x
  composeList (f:fs) = foldr (.) f fs
