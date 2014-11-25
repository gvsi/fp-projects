import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

f :: String -> Int
f xs = sum [(digitToInt n)*3^i | (n,i) <- zip (reverse xs) [0..]]

g :: String -> Int
g xs = help (reverse xs) 0
	where
		help [] _ = 0
		help (x:xs) i = (digitToInt x) * 3 ^ i + help xs (i+1)

p :: [Int] -> Bool
p xs | xs /= [] && head xs /= 0 = all (\x -> x `mod` head xs == 0) (filter (>=0) (tail xs))
	 | otherwise = error "Error"

q :: [Int] -> Bool
q xs | xs /= [] && head xs /= 0 = help (tail xs)
	 | otherwise = error "Error"
	where
		help [] = True
		help (y:ys) | y >= 0 = y `mod` head xs == 0 && help ys
					| otherwise = help ys

r :: [Int] -> Bool
r xs | xs /= [] && head xs /= 0 = foldr (&&) True (map (\x -> x `mod` head xs == 0) (filter (>=0) (tail xs)))
     | otherwise = error "Error"

prop_pq :: [Int] -> Bool
prop_pq xs = p xs == q xs && r xs == q xs

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn X          = ["X"]
rpn (Const n)  = [show n]
rpn (Neg p)    = rpn p ++ ["-"]
rpn (p :+: q)  = rpn p ++ rpn q ++ ["+"] 
rpn (p :*: q)  = rpn p ++ rpn q ++ ["*"]

-- 3 b

evalrpn :: [String] -> Int -> Int
evalrpn = undefined