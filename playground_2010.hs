import Data.Char
import Test.QuickCheck

f :: Char -> Int
f char | elem char ['a'..'m'] = 1
	   | elem char ['n'..'z'] = 2
	   | elem char ['A'..'M'] = 3
	   | elem char ['N'..'Z'] = 6
	   | otherwise = error "Thanks, Martin"

g :: String -> Int
g xs = sum [f x | x <- xs, isAlpha x]

h :: String -> Int
h [] = 0
h (x:xs) | isAlpha x = f x + h xs
		 | otherwise = h xs

c :: [Int] -> Bool
c [_] = True
c xs = and [a > b | (a,b) <- zip xs (tail xs)]

d :: [Int] -> Bool
d [] = True
d [_] = True
d (x:xs) | x > (head xs) = True && d xs
		 | otherwise = False

prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs

prop_gh :: String -> Bool
prop_gh xs = g xs == h xs