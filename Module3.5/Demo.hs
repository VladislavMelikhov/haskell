module Demo where

import Prelude hiding(foldl,any)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini [] = ini
foldl f ini (x:xs) = foldl f ini' xs
    where ini' = f ini x


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f ini [] = ini
foldl' f ini (x:xs) = ini' `seq` foldl' f ini' xs
    where ini' = f ini x

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x b -> p x || b) False

sumProd :: Num a => [a] -> (a, a)
sumProd = foldr (\x (s,p) -> (x+s,x*p)) (0,1)

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s,c) -> (s+x,c+1)) (0,0)

evenOnly :: [a] -> [a]
evenOnly = foldr (\(i,x) b -> if (odd i) then b else x : b) [] . zip [1..]

evenOnly' :: [a] -> [a]
evenOnly' = map snd . filter (\(i,x) -> even i) . zip [1..]

evenOnly'' :: [a] -> [a]
evenOnly'' = snd . foldr (\a (xs,ys) -> (a : ys, xs)) ([],[])