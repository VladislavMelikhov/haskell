module Demo where

import Prelude hiding (foldr)

sumList :: [Integer] -> Integer
sumList = foldr (+) 0

productList :: [Integer] -> Integer
productList []     = 1
productList (x:xs) = x * productList xs

concatList :: [[a]] -> [a]
concatList = foldr (++) []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []     = ini
foldr f ini (x:xs) = x `f` foldr f ini xs 

sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares = foldr f 0
    where
        f x s | x > 0     = x^2 + s  
              | otherwise = s

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0


--f x s = const (succ s) x
--f = const succ

sumOdd :: [Integer] -> Integer
sumOdd = foldr (+) 0 . filter odd
