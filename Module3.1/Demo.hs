module Demo where

import Prelude hiding (length, (++), null, last, init, reverse, zip, zip3, unzip, take, drop, splitAt, (!!))

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements first second list = first : second : list

nTimes :: a -> Int -> [a]
nTimes element n = helper element n []
    where
        helper :: a -> Int -> [a] -> [a]
        helper element 0 list = list
        helper element n list = helper element (n - 1) (element : list)

second :: [a] -> a
second = head . tail

second' :: [a] -> a
second' (_ : xs) = head xs

second'' :: [a] -> a
second'' (_ : x : _) = x

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)

null :: [a] -> Bool
null [] = True
null _  = False

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) = if (odd x) then x : oddsOnly xs else oddsOnly xs

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs

init :: [a] -> [a]
init [] = error "Error!!"
init [x] = []
init (x:xs) = x : init xs

reverse :: [a] -> [a]
reverse l = rev l [] where
    rev [] a     = a
    rev (x:xs) a = rev xs (x:a) 

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _ _ _ = []

unzip :: [(a,b)] -> ([a],[b])
unzip []       = ([],[])
unzip ((x,y):xys) =
    let (xs,ys) = unzip xys
    in (x:xs,y:ys)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a:as) (b:bs) (c:cs) = (a + b + c) : sum3 as bs cs
sum3 [] (b:bs) (c:cs) = (b + c) : sum3 [] bs cs
sum3 (a:as) [] (c:cs) = (a + c) : sum3 as [] cs
sum3 (a:as) (b:bs) [] = (a + b) : sum3 as bs []
sum3 (a:as) [] [] = a : sum3 as [] []
sum3 [] (b:bs) [] = b : sum3 [] bs []
sum3 [] [] (c:cs) = c : sum3 [] [] cs
sum3 [] [] [] = []

groupElems :: Eq a => [a] -> [[a]]
groupElems list = reverse (groupElemsHelper list [])
    where 
        groupElemsHelper :: Eq a => [a] -> [[a]] -> [[a]]
        groupElemsHelper (x:xs) [] = groupElemsHelper xs [[x]]
        groupElemsHelper [] list = list
        groupElemsHelper (x:xs) (y:ys) =
            if x == head y then groupElemsHelper xs ((x:y):ys)
                           else groupElemsHelper xs ([x]:y:ys)

groupElems' :: Eq a => [a] -> [[a]]
groupElems' [] = []
groupElems' [x] = [[x]]
groupElems' (x:xs) = if x == head xs then
                            let (r:rs) = groupElems' xs
                            in (x:r):rs
                     else [x] : groupElems' xs

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (_:xs)      = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

xs     !! n | n < 0 = error "Prelude.!!: negative index"
[]     !! _         = error "Prelude.!!: index is too large"
(x:_)  !! 0         = x
(_:xs) !! n         = xs !! (n - 1)