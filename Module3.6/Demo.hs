module Demo where

import Prelude hiding(foldl1, foldr1, maximum, foldl, scanl, foldr, scanr, iterate)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x]    = x
foldr1 f (x:xs) = x `f` foldr1 f xs
foldr1 f []     = error "foldr1: EmptyList"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 f []     = error "foldl1: EmptyList"

maximum :: (Ord a) => [a] -> a
maximum = foldl1 max

lastElem :: [a] -> a
lastElem = foldl1 $ flip const

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini []     = ini
foldl f ini (x:xs) = foldl f (ini `f` x) xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f ini []     = [ini]
scanl f ini (x:xs) = ini : scanl f (ini `f` x) xs

facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1..]

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

--Demo> take 10 . partialSums . map(**(-1)) $ facs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []     = ini
foldr f ini (x:xs) = x `f` (foldr f ini xs)

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f ini []     = [ini]
scanr f ini (x:xs) = (x `f` q) : qs
                        where qs@(q:_) = scanr f ini xs

unfold :: (b -> (a,b)) -> b -> [a]
unfold f ini = let (x,ini') = f ini in
    x : unfold f ini'

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\x -> (x, f x))

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
    helper (Just (x,ini')) = x : unfoldr f ini'
    helper Nothing         = []

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
    where g (first, last) | first > last = Nothing 
                          | otherwise    = Just (last, (first, pred last))