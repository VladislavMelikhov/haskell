module Demo where

import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

--runWriter :: Writer w a -> (a, w)

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter = snd . runWriter

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k =
        let (x,u) = runWriter m
            (y,v) = runWriter $ k x
        in Writer (y, u `mappend` v)

tell :: Monoid w => w -> Writer w ()
tell w = writer ((),w)

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
    let res = op arg1 arg2
    tell "ok "
    if abs res < 128 then
        return res
    else do
        tell "overflow"
        return res

type Shopping = Writer ([String], Sum Integer) ()

purchase :: String -> Integer -> Shopping 
purchase item cost = tell ([item], Sum cost)

total :: Shopping -> Integer
total = getSum . snd . execWriter

items :: Shopping -> [String]
items = fst . execWriter

shopping1 :: Shopping
shopping1 = do
    purchase "Jeans"    19200
    purchase "Water"      180
    purchase "Lettuce"    328