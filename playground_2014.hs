import Data.Char
import Test.QuickCheck

f :: Char -> Bool
f char | elem char ['a'..'m'] || elem char ['A'..'M'] = True
	   | otherwise = False

g :: String -> Bool
g str = length [f x | x <- str, x] > length [f x | x <- str, not x]

h :: String -> Bool
h xs = count xs 0 0
	   where
	   count :: String -> Int -> Int -> Bool
	   count [] trues falses = trues > falses
	   count (x:xs) trues falses | f x = count xs (trues+1) falses
		   					     | otherwise = count xs trues (falses+1)


c xs = [a | (a,b) <- zip xs (tail xs), a == b]

d :: [Int] -> [Int]
d [_] = []
d [] = []
d (x:y:xs) | x == y = x : d xs
		   | otherwise = d xs
--d (x:xs) | x == head (xs) = x : d xs
--		 | otherwise = d xs

prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs