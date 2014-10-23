import Data.Char
import Test.QuickCheck

f :: Char -> Int
f ch | elem ch ['0'..'9'] = digitToInt ch
     | elem (toLower ch) ['a'..'f'] = ord ch - 87
     | otherwise = error "not an hexadecimal digit"

isHex :: Char -> Bool
isHex ch = elem ch ['0'..'9'] || elem (toLower ch) ['a'..'f']

g :: String -> Int
g str = maximum ([f ch | ch <- str, isHex ch] ++ [-1])

h :: String -> Int
h [] = -1
h (x:xs) | isHex x = f x `max` (h xs)
		 | otherwise = h xs

c :: [Int] -> Int
c [] = error "no ints!!"
c [_] = 1
c xs = product [ a-b | (a,b) <- zip xs (tail xs)]

d :: [Int] -> Int
d [] = error "no ints!!"
d [_] = 1
d [x,y] = x - y
d (x:y:xs) = (x-y) * d (y:xs)

prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs