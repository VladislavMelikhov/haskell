module Demo where

roots :: Double -> Double -> Double -> (Double, Double)

roots a b c =
    (
        (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    ,
        (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
    )


roots' a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) 
    in (
        (-b - d) / (2 * a)
    ,
        (-b + d) / (2 * a)
    )

roots'' a b c =
    let { d = sqrt (b ^ 2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a) }   
    in (x1, x2)
    
roots''' a b c =
    let 
        x1 = (-b - d) / aTwice
        x2 = (-b + d) / aTwice
        d = sqrt (b ^ 2 - 4 * a * c)
        aTwice = 2 * a   
    in (x1, x2)

roots'''' a b c = (x1, x2) where
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d = sqrt (b ^ 2 - 4 * a * c)
    aTwice = 2 * a
    
factorial :: Integer -> Integer
factorial n | n >= 0 = let
                    helper :: Integer -> Integer -> Integer
                    helper acc 0 = acc
                    helper acc n = helper (acc * n) (n - 1)
                in helper 1 n 
            | otherwise = error "Arg must be >= 0" 

factorial7 :: Integer -> Integer
factorial7 n | n >= 0    = helper 1 n  
             | otherwise = error "Arg must be >= 0"
    where
        helper :: Integer -> Integer -> Integer
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
        
            
            
rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = let
    (x1, x2) = roots a b c
    in x2 - x1

seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 0 = let
        helper prev curr next 2 = next
        helper prev curr next n = helper curr next (next + curr - 2 * prev) (n - 1)
        in helper 1 2 3 n
       | otherwise = error "Arg must be >= 0"

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | x > 0  = helper 0 0 x
              | x < 0  = helper 0 0 (-x)
    where
        helper sum count 0 = (sum, count)
        helper sum count x = helper (sum + mod x 10) (count + 1) (div x 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    helper :: (Double -> Double) -> Double -> Double -> Double -> Double -> Integer -> Double
    helper func sum x1 x2 delta 0 = sum
    helper func sum x1 x2 delta n = helper (func) (sum + (func x1 + func x2) * (x2 - x1) / 2) (x2) (x2 + delta) delta (n - 1)    
    d = (b - a) / 1000
    in helper (f) 0 a (a + d) d 1000