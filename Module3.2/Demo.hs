module Demo where

import Prelude hiding (filter, takeWhile, dropWhile, span, break, map, concat, concatMap, and, or, all, any, zipWith, zipWith3)
import Data.Char

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
    | p x       = dropWhile p xs'
    | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a],[a])
span p xs = (takeWhile p xs, dropWhile p xs)

break :: (a -> Bool) -> [a] -> ([a],[a])
break p xs = span (not . p) xs

readDigits :: String -> (String,String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs) 
    | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
    | otherwise        = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = (qsort less) ++ [x] ++ (qsort greaterOrEqual)
    where        
         (less,greaterOrEqual) = split (<x) xs
         split :: (a -> Bool) -> [a] -> ([a],[a])
         split _ []     = ([],[])
         split p (x:xs) = let (ys,zs) = split p xs
            in if p x then (x:ys,zs)
                      else (ys,x:zs)
        
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
                
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat (map f xs)

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


--GHCi> perms [1,2,3]
--[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = let res = perms xs 
        in insertAll x res
            where
                insertAll :: a -> [[a]] -> [[a]]
                insertAll elem permutations = concatMap (insert elem) permutations

                insert :: a -> [a] -> [[a]]
                insert elem list = insertHelper 0 elem list list []

                insertHelper :: Int -> a -> [a] -> [a] -> [[a]] -> [[a]]
                insertHelper pos elem list [] ans = (insertAt pos elem list) : ans
                insertHelper pos elem list (x:xs) ans = insertHelper (pos + 1) elem list xs ((insertAt pos elem list) : ans) 

                insertAt :: Int -> a -> [a] -> [a]
                insertAt pos elem list = let (begin,end) = splitAt pos list in begin ++ [elem] ++ end

--perms :: [a] -> [[a]]
--perms [] = [[]]
--perms [x] = [[x]]
--perms (x:xs) = concatMap (insertElem x) (perms xs) where
--			insertElem x [] = [[x]]
--			insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)

and, or :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or []     = False
or (x:xs) = x || or xs

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

revWords :: String -> String
revWords = unwords . map reverse . words

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = []


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max x (max y z))