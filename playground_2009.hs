import Data.Char
import Test.QuickCheck

f :: Char -> Char

alpha = ['A'..'Z']
f ch | elem ch alpha = head [b | (a,b) <- zip alpha (reverse alpha), a==ch ]
	 | otherwise = error "sorry"

isInAlphabet :: Char -> Bool
isInAlphabet ch = elem ch alpha

g :: String -> String
g xs = [f x | x <- xs, isInAlphabet x]

h :: String -> String
h [] = []
h (x:xs) | isInAlphabet x = f x : h xs
		 | otherwise = h xs 


c :: String -> String
c [] = []
c xs = [x | (x, n) <- zip xs [1..], n `mod` 2 == 1]

d :: String -> String
d [] = []
d xs = check xs 1
	where
	check :: String -> Int -> String
	check [] n = []
	check (x:xs) n | n `mod` 2 == 1 = x : check xs (n+1)
				   | otherwise = check xs (n+1) 

prop_cd :: String -> Bool
prop_cd xs = c xs == d xs