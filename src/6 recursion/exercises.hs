-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3
(^) :: Int -> Int -> Int
m ^ 0 = 1
0 ^ n = 0
m ^ n = m * (m Main.^ (n-1))

-- 4
euclid :: Int -> Int -> Int
euclid m n | m == n    = n
           | m > n     = euclid n (m-n)
           | otherwise = euclid m (n-m)

-- 6
and2 :: [Bool] -> Bool
-- doesn't make sense for empty list
and2 (x:xs) | null xs   = x
            | otherwise = x && (and2 xs)

concat2 :: [[a]] -> [a]
concat2 lst | null lst  = []
            | otherwise = (head lst) ++ (concat2 (tail lst))

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n-1) x

selectNth :: [a] -> Int -> a
-- assume non-empty list and index not greater than length
selectNth (x:_) 0  = x
selectNth (_:xs) n = selectNth xs (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 a [] = False
elem2 a (x:xs) = (a==x) || elem2 a xs

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8
splitAtHalf :: [a] -> ([a], [a])
splitAtHalf xs = splitAt n xs
                   where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort xs | length xs < 2 = xs
         | otherwise     = merge (msort firstHalf) (msort secondHalf)
                           where splitted = splitAtHalf xs
                                 firstHalf = fst splitted
                                 secondHalf = snd splitted

-- 9
sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2(xs)

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 n (x:xs) = x : take2 (n-1) xs

last2 :: [a] -> a
last2 xs | length xs == 1 = head xs
         | otherwise      = last2 (tail xs)
