module Lists where

  -- I've left this sample definition in here, you should delete it
  oddNumbers :: Int -> [Int]
  oddNumbers n = [ x | x <- [1..n], (mod x 1) == 0] 

  -- Write a function to build an (unbounded) list of counting numbers (counting numbers start at 1). The counting numbers are the set {1, 2, 3, ...}. You may assume n is a positive integer.
  countingNumbers :: Integer -> [Integer]
  countingNumbers n = [1..n]
