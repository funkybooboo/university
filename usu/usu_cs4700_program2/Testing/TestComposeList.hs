composeList :: [(a -> a)] -> (a -> a)
composeList [] = \x -> x
composeList (f:fs) = foldr (.) f fs

add1 :: Int -> Int
add1 x = 1 + x

add2 :: Int -> Int
add2 x = 2 + x

add3 :: Int -> Int
add3 x = 3 + x

main :: IO ()
main = do

  putStrLn "Testing composeList with: composeList [add1, add2, add3] 3 -> 9"
  print (composeList [add1, add2, add3] 3)

  putStrLn "Testing composeList with: composeList [] 3 -> 3"
  print (composeList [] 3)