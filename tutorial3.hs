-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 16/17 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers xs = map toUpper xs

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (\x -> fromIntegral x / 100) xs

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers' xs



-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b.
rmChar ::  Char -> String -> String
rmChar ch str = filter (/=ch) str

-- c.
above :: Int -> [Int] -> [Int]
above x xs = filter (>x) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter (\(a,b) -> a /= b) xs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp ch xs = [x | x <- xs, x /= ch]

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch xs = rmChar ch xs == rmCharComp ch xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' s = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\s -> even (length s)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)

rmCharsFold :: String -> String -> String
rmCharsFold xs str = foldr rmChar str xs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform xs = all ( == head(xs) ) (tail xs)

-- b.
valid :: Matrix -> Bool
valid m = length m > 0 && uniform (map length m)

-- 6.
zipWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithList f xs ys = [f x y | (x,y) <- zip xs ys]

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM xs ys = zipWith (zipWith (+)) xs ys

-- 8.
--timesM :: Matrix -> Matrix -> Matrix
--timesM xs ys = [ | ] zipWith (zipWith (*)) xs ys

-- Optional material
-- 9.
