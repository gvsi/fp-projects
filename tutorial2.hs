-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs | n >= 0 && n <= length xs = drop n xs ++ take n xs
			| otherwise = error "Try with another int"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alfa (rotate n alfa)
			where alfa = ['A'..'Z']

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp x xs = head ([z | (w,z) <- xs, x == w ] ++ [x])

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec l [] = l
lookUpRec l ((w,z):xs) | w == l = z
					   | otherwise = lookUpRec l xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp l xs = lookUp l xs == lookUpRec l xs 

-- 5.
encipher :: Int -> Char -> Char
encipher n l = lookUp l (makeKey n)

-- 6.
normalize :: String -> String
normalize xs = map toUpper (filter isAlphaNum xs)

-- 7.
encipherStr :: Int -> String -> String
encipherStr n xs = map (encipher n) (normalize xs)

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(w,z) | (z,w) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((w,z):xs) = (z,w) : reverseKey xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs
-- 9.
decipher :: Int -> Char -> Char
decipher n l = lookUp l (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n xs = map (decipher n) xs

-- 10.
contains :: String -> String -> Bool
contains container contained = [x | x <- (suffixes container), isPrefixOf contained x] /= []
								where suffixes xs = [drop x xs | x <- [0..length xs]]

-- 11.
candidates :: String -> [(Int, String)]
candidates xs = [(n, (decipherStr n xs)) | n <- [1..26], contains (decipherStr n xs) "THE" || contains (decipherStr n xs) "AND"]



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = [take 5 xs] ++ splitEachFive (drop 5 xs)
				 | length xs == 5 = [take 5 xs]
				 | otherwise = [xs ++ (replicate (5 - length xs) 'X')]

-- 13.
prop_transpose :: String -> Bool
prop_transpose xs = splitEachFive xs == transpose ( transpose ( splitEachFive xs ) )
---
-- 14.
encrypt :: Int -> String -> String
encrypt n xs = concat ( transpose ( splitEachFive ( encipherStr n xs ) ) )

-- 15.
decrypt :: Int -> String -> String
decrypt n xs = decipherStr n (concat (transpose (splitIt xs (length xs))))
   where 
   splitIt :: String -> Int -> [String]
   splitIt [] len = []
   splitIt xs len = [take (len `div` 5) xs] ++ (splitIt (drop (len `div` 5) xs) len)

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs xs = nub [(x,length (filter (==x) xs )) | x <- xs]


-- 17
freqDecipher :: String -> [String]
freqDecipher xs = undefined