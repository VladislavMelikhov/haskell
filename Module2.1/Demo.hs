module Demo where

import Data.Function

getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom x1 x2 x3 = x2

mono :: Char -> Char
mono x = x

--semiMono :: Char -> a -> Char
semiMono x y = x

apply2 f x = f (f x)

multSecond = g `on` h
g = (*)
h = snd

sumFstFst = (+) `on` helper
    where helper pp = fst $ fst pp

sumFstFst' = (+) `on` (\pp -> fst $ fst pp)

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x + y + z) `on3` (^2)