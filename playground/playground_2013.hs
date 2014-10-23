import Data.Char
import Test.QuickCheck

f :: Char -> Int
f char | elem char "haskell" = 5
	   | elem char "HASKELL" = 10
	   | elem char ['a'..'z'] = 1
	   | elem char ['A'..'Z'] = 2
	   | otherwise = 0

g :: String -> Int
g str = product (map f (filter isAlpha str))

gRec :: String -> Int
gRec [] = 1
gRec (x:xs) | isAlpha x = f x * gRec xs
			| otherwise = gRec xs

c :: String -> String -> String
c str1 str2 = [ x | (x,y) <- zip str1 str2, x == y]

d :: String -> String -> String
d _ [] = []
d [] _ = []
d (x:xs) (y:ys) | x == y = x : d xs ys
				| otherwise = d xs ys

prop_cd :: String -> String -> Bool
prop_cd xs ys = c xs ys == d xs ys 