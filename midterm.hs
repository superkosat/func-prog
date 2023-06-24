--Alec Sirkin, Function Programming Midterm
--aws4934, N19233633

--Exercise 1 & 2: quicksort
--quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:xs) =
    let lowerHalf = quicksort [n | n <- xs, n <= pivot]
        upperHalf = quicksort [n | n <- xs, n > pivot]
    in lowerHalf ++ [pivot] ++ upperHalf
 --when the type signature I have above is commented out,
 --it reverts to ord a => [a] -> [a] because it orders
 --a list of, in that case, a non specified type.


 --Now, without the type signature of a specified type,
 --the function will take a list of any type and will order
 --it based upon the type's specified ordering. This of course also
 --applies to the String type as well. Thus, a string will be
 --accepted as a list of characters ([Char]), and will be
 --reordered alphabetically as the ordering of characters is
 --based upon their ASCII values

anagram :: String -> String -> Bool
anagram "" "" = True
anagram str1 str2
    | sortedStr1 == sortedStr2 = True
    | otherwise = False
    where sortedStr1 = quicksort str1
          sortedStr2 = quicksort str2


--Exercise 3: Binary Search Trees

data Tree = Nil | Node Int Tree Tree

--3a
leaf :: Int -> Tree
leaf k = Node k Nil Nil

--3b
tree :: Tree
tree = Node 5 (Node 7 Nil Nil)
    (Node 2 (Node 1 (Node 5 Nil Nil) Nil) (Node 3 Nil Nil))

bstree = Node 2 (Node 1 Nil Nil)
    (Node 5 (Node 5 (Node 3 Nil Nil) Nil) (Node 7 Nil Nil))

--3c
contains :: Int -> Tree -> Bool
contains _ Nil = False
contains k (Node x l r)
    | k == x = True
    | otherwise = (contains k l) || (contains k r)

--3d
minInt = minBound :: Int
maxInt = maxBound :: Int

--3e
minabc :: Int -> Int -> Int -> Int
minabc a b c
    | a < b && a < c = a
    | b < a && b < c = b
    | c < a && c < b = c

maxabc :: Int -> Int -> Int -> Int
maxabc a b c
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c

--3f
minTree :: Tree -> Int
minTree Nil = maxInt
minTree (Node x Nil Nil) = x
minTree (Node x l r) =
    let leftMin = minTree l
        rightMin = minTree r
    in minimum [x, leftMin, rightMin]

maxTree :: Tree -> Int
maxTree Nil = minInt
maxTree (Node x Nil Nil) = x
maxTree (Node x l r) =
    let leftMax = maxTree l
        rightMax = maxTree r
    in maximum [x, leftMax, rightMax]

--3g
isBST :: Tree -> Bool
isBST Nil = True
isBST (Node x l r) =
    let leftBST = all (\y -> y <= x) (inOrder l)
        rightBST = all (\y -> y >= x) (inOrder r)
    in leftBST && rightBST && isBST l && isBST r
  where
    inOrder Nil = []
    inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r 
    --generates inorder traversal of tree as a list

--3h
containsBST :: Int -> Tree -> Bool
containsBST _ Nil = False
containsBST k (Node x l r)
    | k == x = True
    | k < x = contains k l
    | otherwise = contains k r

    --The function returns False when the previous 'contains'
    --function returns true because unlike the previous function,
    --containsBST assumes the tree is a binary search tree and
    --thus, does not check any subtrees to the left of the root 
    --as the root is less than 7.

--3i
insert :: Int -> Tree -> Tree
insert k Nil = (Node k Nil Nil)
insert k (Node x l r)
    | k <= x = Node x (insert k l) r
    | otherwise = Node x l (insert k r)

--3j
extract :: Tree -> Maybe (Int, Tree)
extract Nil = Nothing
extract (Node x l r)
  | Nil <- l = Just (x, r)
  | Just (minVal, newl) <- extract l = Just (minVal, Node x newl r)

--3k
removeroot :: Tree -> Maybe Tree
removeroot Nil = Nothing
removeroot (Node _ Nil r) = Just r
removeroot (Node _ l Nil) = Just l
removeroot (Node _ l r)
  | Just (minVal, newr) <- extract r = Just (Node minVal l newr)

--3l
--delete :: Int -> Tree -> Tree


main = do
    let test = ["D", "C", "B", "A"]
    let str = "hello"
    --print(quicksort str)
    --print(anagram "silent" "listen")
    --print(anagram "hello" "hole")
    --print(contains 3 tree)
    --print(contains 6 tree)
    --print(minabc 1 9 3)
    --print(minTree tree)
    --print(minTree bstree)
    --print(maxTree tree)
    --print(maxTree bstree)
    print(isBST tree)
    print(isBST bstree)