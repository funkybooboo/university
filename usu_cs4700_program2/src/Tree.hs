module Tree where

data BinarySearchTree a 
  = Empty
  | Branch (BinarySearchTree a) a (BinarySearchTree a)

{-- reduce solution
blanks = "." : blanks

makeIndentations maxHeight Empty = Empty
makeIndentations maxHeight (Branch left x right) =
    let indent = take (maxHeight - (height (Branch left x right))) blanks
    in Branch
         (makeIndentations maxHeight left)
         (foldr (++) indent) ++ (show x) ++ "\n"
         (makeIndentations maxHeight right)

prettyFormat tree = reduceTree "" (\this left right -> left ++ this ++ right) (makeIndentations (height tree) tree) 
--}

{-- Create a String picture of the tree.  --}
prettyFormat tree = prettyFormatHelper "" tree
prettyFormatHelper indent Empty = ""
prettyFormatHelper indent (Branch left value right) = 
  let nextIndent = "." ++ indent
  in (prettyFormatHelper nextIndent left) 
    ++ indent ++ (show value) ++ "\n"
    ++ (prettyFormatHelper nextIndent right) 

{-- Apply a function to every node in the tree.  --}
mapTree func Empty = Empty
mapTree func (Branch left value right) = (Branch (mapTree func left) (func value) (mapTree func right))

{-- Reduce the tree to a single value.  --}
reduceTree initialValue interiorFunc Empty = initialValue
reduceTree initialValue interiorFunc (Branch left value right) =
  interiorFunc 
     value 
     (reduceTree initialValue interiorFunc left) 
     (reduceTree initialValue interiorFunc right)
  

{-- Figure out the height of the tree --}
height tree = reduceTree 0 (\_ left right -> 1 + (max left right)) tree

{-- What is maximum value in tree? --}
maxInTree value Empty = value
maxInTree value (Branch left x right) = max (maxInTree x left) (maxInTree x right)

{-- What is minimum value in tree? --}
minInTree value Empty = value
minInTree value (Branch left x right) = min (minInTree x left) (minInTree x right) 

{-- Value must be greater than max in left tree and min in right tree --}
check Empty = True
check (Branch left value right) = 
  (check left) && (check right) && ((maxInTree value left) <= value) && ((minInTree value right) >= value)

{-- reduce solution
find x tree = reduceTree False (\this left right -> x == this || left || right) tree
--}

{-- Find a value in the tree --}
find x Empty = True
find x (Branch left value right) = False

{-- Balance a binary tree --}
balance tree | height tree < 2 = tree
balance (Branch left x right) =
  {-- First balance children then balance the node if needed --}
  balanceIfNeeded (Branch (balance left) x (balance right))

{-- A node needs to be balanced if the height of the left and right children differ by two or more.  --}
balanceIfNeeded (Branch left x right)
  | (height left) > ((height right) + 1) = balanceIfNeeded (leftCase (Branch left x right))
  | (height right) > ((height left) + 1) = balanceIfNeeded (rightCase (Branch left x right))
  | otherwise = (Branch left x right)

{-- Node is unbalanced to the left, apply either a left left or left right rotation.  --}
leftCase (Branch (Branch b a c) x right)
  | (height b) > (height c) = leftLeftCase (Branch (Branch b a c) x right)
  | otherwise = leftRightCase (Branch (Branch b a c) x right)

{-- Node is unbalanced to the right, apply either a right left or right right rotation.  --}
rightCase (Branch left x (Branch b a c))
  | (height b) > (height c) = rightLeftCase (Branch left x (Branch b a c))
  | otherwise = rightRightCase (Branch left x (Branch b a c))

leftLeftCase (Branch (Branch (Branch d v2 c) v3 b) v5 a) =
   Branch (Branch d v2 c) v3 (Branch b v5 a)
rightRightCase (Branch a v3 (Branch b v5 (Branch c v7 d))) =
   Branch (Branch a v3 b) v5 (Branch c v7 d)
leftRightCase (Branch (Branch b v3 (Branch c v4 d)) v5 a) =
   Branch (Branch b v3 c) v4 (Branch d v5 a) 
rightLeftCase (Branch a v3 (Branch (Branch d v4 c) v5 b)) =
   Branch (Branch a v3 d) v4 (Branch c v5 b) 
