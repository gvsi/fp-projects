-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (2/3 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, even x ]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = x `div` 2 : halveEvensRec xs
					 | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, lo <= x, hi >= x]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | lo <= x, hi >= x = x : inRangeRec lo hi xs
						| otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs

-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + countPositivesRec xs
						 | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round (fromIntegral x - fromIntegral x / 10)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [ discount x | x <- xs, discount x < 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount x < 19900 = discount x + pennypincherRec xs
					   | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
					 | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise xs = toUpper (head xs) : [ toLower x | x <- tail xs]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec xs = toUpper (head xs) : allLower (tail xs)
allLower [] = []
allLower (x:xs) = toLower x : allLower xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
capitaliseLong xs | length xs >= 4 = capitalise xs
				  | otherwise = [ toLower x | x <- xs]

title [] = []
title xs = capitalise (head xs) : [ capitaliseLong x | x <- tail xs]

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitaliseRec x : titleAuxRec xs

titleAuxRec [] = []
titleAuxRec (x:xs) | length x >= 4 = capitaliseRec x : titleAuxRec xs
				   | otherwise = allLower x : titleAuxRec xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs 




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len words = [ x | x <- words, length x == len, inPosition < length x, inPosition >= 0, x !! inPosition == letter, len > 0]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter inPosition len [] = []
crosswordFindRec letter inPosition len words | len > 0, inPosition >= 0 = crosswordFindAuxRec letter inPosition len words
											 | otherwise = []

crosswordFindAuxRec letter inPosition len [] = []
crosswordFindAuxRec letter inPosition len (x:xs) | length x == len, inPosition < length x, x !! inPosition == letter = x : crosswordFindRec letter inPosition len xs
												 | otherwise = crosswordFindAuxRec letter inPosition len xs

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter inPosition len words = crosswordFind letter inPosition len words == crosswordFindRec letter inPosition len words



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str letter = [ num | (x, num) <- (zip str [0..]), letter == x]

-- Recursive version

searchRec :: String -> Char -> [Int]
searchRec str letter = searchAuxRec str letter 0

searchAuxRec :: String -> Char -> Int -> [Int]
searchAuxRec [] letter position = []
searchAuxRec (x:xs) letter position | letter == x = position : searchAuxRec xs letter (position + 1)
									| otherwise = searchAuxRec xs letter (position + 1)

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str letter = search str letter == searchRec str letter


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
suffixes xs = [ drop x xs | x <- [0..(length xs)]]
contains container contained = [ x | x <- suffixes container, isPrefixOf contained x] /= []

-- Recursive[] version
containsRec :: String -> String -> Bool
containsRec container contained = containsAuxRec container contained 0
containsAuxRec container contained count | isPrefixOf contained (drop count container) = True
										 | length container - 1 == count = False
										 | length container == 0 = False
										 | otherwise = containsAuxRec container contained (count + 1)

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains container contained = contains container contained == containsRec container contained