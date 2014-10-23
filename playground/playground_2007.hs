import Data.Char
import Test.QuickCheck

f :: Char -> Int
f x | isUpper x = ord x - ord 'A'
	| isLower x = ord x - ord 'a'
	| otherwise = error "sorry"

g :: String -> Int
g xs = sum [f x | x <- xs, isAlpha x]

h :: String -> Int
h [] = 0
h (x:xs) | isAlpha x = f x + h xs
		 | otherwise = h xs

c :: [Int] -> [Int] -> [Int]
c xs ys | length xs == length ys = [ a-b | (a,b) <- zip xs ys]
		| otherwise = error "not same length"

d :: [Int] -> [Int] -> [Int]
d [] _ = []
d _ [] = []
d (x:xs) (y:ys) = (x-y) : d xs ys

e :: [Int] -> [Int] -> Bool
e xs ys = and [True | x <- c xs ys, x == 0]