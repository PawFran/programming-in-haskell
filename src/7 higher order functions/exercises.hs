-- foldr recursion pattern
-- f [] = v
-- f (x:xs) = x # f xs

-- foldl recursion pattern (accumulator)
-- f v [] = v
-- f v (x:xs) = f (v # x) xs

-- leniwa ewaluacja pozwala unikac iteracji na z gory zadanym zakresie: np. generator potencjalnie nieskonczonej listy zawsze wyprodukuje element kiedy ten bedzie potrzebny, wiec nie trzeba ustalac zakresu

import Data.Function
-- 1`
f :: Int -> Int
f x = x + 10

p :: Int -> Bool
p x = even x

xs = [1..10]
ys = [f x | x <- xs, p x]

ys2 = filter p xs & map f

-- 2
all2 :: (a -> Bool) -> [a] -> Bool
all2 p xs = map p xs & foldr (&&) True

any2 :: (a -> Bool) -> [a] -> Bool
any2 p xs = map p xs & foldr (||) False

-- c and d
l = [2, 2, 4, 3, 2]

is :: (a -> Bool) -> [a] -> [Bool]
is p xs = map p xs

allNextInclusive :: [Bool] -> [Bool]
allNextInclusive [] = []
allNextInclusive (x:xs) | null xs   = [x]
                        | otherwise = ( x && (and xs) ) : (allNextInclusive xs)

allPreviousInclusive = reverse . allNextInclusive . reverse

zippedWithCondition p xs = zip xs (allPreviousAreTrueInclusive p xs) 
                           where allPreviousAreTrueInclusive p xs = is p xs & allPreviousInclusive

takeWhile2 p xs = zippedWithCondition p xs & filter snd & map fst
dropWhile2 p xs = zippedWithCondition p xs & filter (not . snd) & map fst

-- 3
map2 f = foldr (\x y -> f x : y) []
filter2 p = foldr (\x y -> if p x then x : y else y) []

-- 4
dec2Int :: [Int] -> Int
dec2Int l = map show l & foldl (++) "" & read :: Int

--5 
