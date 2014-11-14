-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 13/14 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (x :#: y) = split x ++ split y
split Sit = []
split x = [x] 

-- 1b. join
join :: [Command] -> Command
join = foldl1 (:#:)

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent x y = split x == split y

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join x = equivalent (join (split x)) x 

prop_split :: Command -> Bool
prop_split x = all accepted $ split x
  where
    accepted Sit     = False
    accepted (_:#:_) = False
    accepted _       = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n x = join $ replicate n x

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon l = copy 5 (Go l :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon l n = copy n (Go l :#: Turn (360.0 / fromIntegral n))

-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral l n s a = (Go l :#: Turn a) :#: spiral (l+s) (n-1) s a



-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise x = join ( help ( split x ) )
  where
    help []                      = []
    help (Go x : Go y : xs)      = help $ Go (x + y) : help xs
    help (Go 0 : xs)             = help xs
    help (Turn x : Turn y : xs)  = help $ Turn (x + y) : help xs
    help (Turn 0 : xs)           = help xs
    help (x:xs)                  = x : help xs

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
      f 0 = Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = Go 10
      g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
      n = Turn 60
      p = Turn(-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
	where
		f 0 = Go 10
		f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
		n = Turn 60
		p = Turn (-60)
-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where
      l 0 = Sit
      l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
      r 0 = Sit
      r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
      f = GrabPen black :#: Go 10
      n = Turn 90
      p = Turn(-90)

