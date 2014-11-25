-- Informatics 1 Functional Programming
-- December 2011

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below

-- 1

-- 1a

f :: [Int] -> Int
f =  undefined

-- 1b

g :: [Int] -> Int
g =  undefined

-- 2

-- 2a

p :: [Int] -> Int
p xs | even (length xs) = sum [(xs !! (i+1)) * (xs !! i) | i <- [0,2..length xs - 1]]
     | otherwise = error "Invalid"

-- 2b

q :: [Int] -> Int
q [] = 0
q [_] = error "Invalid"
q (x:y:zs) = x * y + q zs

-- 2c

r :: [Int] -> Int
r xs | even (length xs) = foldr (+) 0 (map (\i -> xs !! i * (xs !! (i+1))) [0,2..length xs -1])
     | otherwise = error "Invalid"
-- 3

data Expr = Var String
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Expr

instance Arbitrary Expr where
  arbitrary = sized arb
    where
    arb 0          =  liftM Var arbitrary
    arb n | n > 0  =  oneof [liftM Var arbitrary,
                             liftM2 (:+:) sub sub, 
                             liftM2 (:*:) sub sub] 
      where
      sub = arb (n `div` 2)

-- 3a

isNorm :: Expr -> Bool
isNorm (Var x) =  True
isNorm (a :*: b) = isTerm (a :*: b)
isNorm (a :+: b) = isNorm a && isNorm b

isTerm :: Expr -> Bool
isTerm (Var x) = True
isTerm (a :*: b) = isTerm a && isTerm b
isTerm (a :+: b) = False


-- 3b

norm :: Expr -> Expr
norm exp | isNorm exp = exp
         | otherwise = help exp
  where
    help ((a :+: b) :*: c) = norm (a :*: c) :+: norm (b :*: c)
    help (a :*: (b :+: c)) = norm (a :*: b) :+: norm (a :*: c)