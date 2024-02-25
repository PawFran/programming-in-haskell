-- 1
data Nat = Zero | Succ Nat
            deriving Show

add :: Nat -> Nat -> Nat
add Zero n      = n
add (Succ m) n  = Succ (add m n) 

mult :: Nat -> Nat -> Nat
mult Zero _     = Zero
mult (Succ m) n = add n (mult m n)

-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = x == y || occurs x l || occurs x r

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y)                  = compare x y == EQ
occurs2 x (Node leftSub y rightSub) | compare x y == LT = occurs2 x leftSub
                                    | compare x y == GT = occurs2 x rightSub
                                    | otherwise         = compare x y == EQ

-- test
x1 :: Tree Int = Leaf 1
x3 :: Tree Int = Leaf 3
x5 :: Tree Int = Leaf 5
x7 :: Tree Int = Leaf 7
x2 :: Tree Int = Node x1 2 x3
x6 :: Tree Int = Node x5 6 x7
fullTree :: Tree Int = Node x2 4 x6
-- occurs2 3 fullTree

-- 3
-- leafsNumber :: Num a1 => Tree a2 => a1
leafsCount (Leaf _)            = 1
leafsCount (Node left _ right) = leafsCount left + leafsCount right

isBalanced :: Tree a -> Bool
isBalanced (Leaf _)               = True
isBalanced (Node left _ right)    = abs(leafsCount left - leafsCount right) <= 1

--4 
splitAtHalf :: [a] -> ([a], [a])
splitAtHalf xs = splitAt (length xs `div` 2) xs

balanced :: [a] -> Tree a
balanced lst | length xs == length ys = let xs = (fst $ splitAtHalf lst)
                                            ys = (snd $ splitAtHalf lst) in
                                                Node (balanced xs) (head y) (balanced ys)
