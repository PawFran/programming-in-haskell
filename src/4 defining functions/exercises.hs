import Data.Function

-- 1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

-- 2
third :: [a] -> a
third2 :: [a] -> a
third3 :: [a] -> a

third xs = xs & tail & tail & head
third2 xs               = xs !! 2
third3 (_ : _ : x : _)  = x

-- 3
safetail :: [a] -> [a]
safetail2 :: [a] -> [a]
safetail3 :: [a] -> [a]

safetail xs = if null xs then xs else tail xs

safetail2 xs | null xs   = xs
             | otherwise = tail xs

safetail3 (_:xs)    = xs
safetail3 _         = []

-- 4
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

disjunction2 :: Bool -> Bool -> Bool
disjunction2 False False = False
disjunction2 _  _ = True

disjunction3 :: Bool -> Bool -> Bool
disjunction3 False b = b
disjunction3 True _ = True

-- 5
conjunction a b = if a then 
                       if b then True else False
                  else False

-- 6
conjunction2 a b = if a then b
                   else False

-- 7
--mult :: Int -> Int -> Int -> Int
--mult x y z = x*y*z
mult = \x -> (\y -> (\z -> x*y*z))

-- 8 
-- assume x is between 0 and 9 (a digit)
luhnDouble :: Int -> Int
luhnDouble x | x > 4 = 2*x - 9
             | otherwise = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble c + luhnDouble a) `mod` 10 == 0
