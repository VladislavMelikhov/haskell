module Demo where

import Prelude hiding (repeat,replicate,cycle,iterate)

fibStream :: [Integer]
fibStream = fibStreamHelper [0] [1]
    where 
        fibStreamHelper :: [Integer] -> [Integer] -> [Integer]
        fibStreamHelper xs@(x:_) ys = x : fibStreamHelper ys (zipWith (+) xs ys)

fib :: [Integer]
fib = zipWith (+) (0:fib) (0:1:fib)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = ys where ys = xs ++ ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat' :: a -> [a]
repeat' = iterate repeatHelper

repeatHelper = id

-------------------------------------------


data Odd = Odd Integer
    deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where
    succ (Odd n)     = Odd (n + 2)
    pred (Odd n)     = Odd (n - 2)
    toEnum n         = Odd (toInteger n)
    fromEnum (Odd n) = fromInteger n
    enumFrom n       = n : enumFrom (succ n)
    enumFromThen first second = first : second : worker (getNext step) (getNext step second) 
        where
            worker :: (Odd -> Odd) -> Odd -> [Odd]
            worker getNextFunction current = current : worker getNextFunction (getNextFunction current)
            step :: Int
            step = fromEnum second - fromEnum first
            getNext :: Int -> Odd -> Odd
            getNext step current | step > 0 = getNext (step - 2) (succ current)
                                 | step < 0 = getNext (step + 2) (pred current)
                                 | otherwise = current
    enumFromTo n'@(Odd n) m'@(Odd m)  | n <= m    = n' : enumFromTo (succ n') m'
                                      | otherwise = [] 
    enumFromThenTo first second last = worker (getNext step) (hasNext step) first last
        where
            step :: Int
            step = fromEnum second - fromEnum first
            getNext :: Int -> Odd -> Odd
            getNext step current | step > 0 = getNext (step - 2) (succ current)
                                 | step < 0 = getNext (step + 2) (pred current)
                                 | otherwise = current
            hasNext :: Int -> Odd -> Odd -> Bool 
            hasNext step (Odd current) (Odd last) | step >= 0  = current <= last
                                                  | otherwise  = current >= last
            worker :: (Odd -> Odd) -> (Odd -> Odd -> Bool)-> Odd -> Odd -> [Odd]
            worker getNextFunction hasNextFunction current last | hasNextFunction current last = current : worker getNextFunction hasNextFunction (getNextFunction current) last
                                                                | otherwise                    = []


test0 = succ (Odd 1) == (Odd 3)
test1 = pred (Odd 3) == (Odd 1)
-- enumFrom
test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- enumFromTo
-- -- По возрастанию
test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- enumFromThen
-- -- По возрастанию
test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- По убыванию
test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- По убыванию
test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- x1 < x3 && x1 > x2
test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- x1 > x3 && x1 < x2
test10 =([Odd 3, Odd 5 .. Odd 1]) == []

allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

----------------------------------------------------------------

--instance Enum Odd where
--    toEnum n = Odd (toInteger n)
--    fromEnum (Odd n) = fromInteger n
--
--    succ (Odd n) = Odd (n + 2)
--    pred (Odd n) = Odd (n - 2)
--
--    enumFrom (Odd n) = map Odd [n,n+2..]
--    enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
--    enumFromThen (Odd n) (Odd n') = map Odd [n,n'..]
--    enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n'..m]

coins :: (Ord a, Num a) => [a]
coins = [2,3,7]

change :: (Ord a, Num a) => a -> [[a]]
change x | x < 0     = []
         | x == 0    = [[]]
         | otherwise = concatMap (\coin -> map (coin :) $ change $ x - coin) coinsS

change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]