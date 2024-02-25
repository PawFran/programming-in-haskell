-- 1
squares :: [Int] = [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]

square2 :: Int -> [(Int, Int)]
square2 n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
replicate2 :: Int -> a -> [a]
replicate2 n x = [x | _ <- [0..n-1]]
-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

-- 7 TODO

-- 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k2,v) <- t, k == k2]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0..]

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
