module Demo where

factorial :: Int -> Int
factorial n = if (n == 0) then 1 else n * factorial (n - 1)

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

factorial'' :: Int -> Int
factorial'' 0 = 1
factorial'' n = if (n < 0) then error "Arg must be >= 0" else n * factorial'' (n - 1)

factorial3 :: Integer -> Integer
factorial3 0 = 1
factorial3 n | n > 0 = n * factorial3 (n - 1)
             | n < 0 = error "Arg must be >= 0"

factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial4 (n - 1)
             | otherwise = error "Arg must be >= 0"

fibonacci' :: Integer -> Integer
fibonacci' n | n == 0 = 0
            | n == 1 = 1
            | n > 1  = fibonacci' (n - 1) + fibonacci' (n - 2)
            | n < 0  = fibonacci' (n + 2) - fibonacci' (n + 1)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n < 0  = fibonacciHelperNegative 0 1 n
            | n > 1  = fibonacciHelperPositive 0 1 n

fibonacciHelperPositive :: Integer -> Integer -> Integer -> Integer
fibonacciHelperPositive prev curr 1 = curr
fibonacciHelperPositive prev curr n = fibonacciHelperPositive (curr) (prev + curr) (n - 1)  

fibonacciHelperNegative :: Integer -> Integer -> Integer -> Integer
fibonacciHelperNegative curr next 0 = curr
fibonacciHelperNegative curr next n = fibonacciHelperNegative (next - curr) (curr) (n + 1)

factorial5 :: Integer -> Integer
factorial5 n | n >= 0    = helper 1 n
             | otherwise = error "arg must be >= 0"

helper :: Integer -> Integer -> Integer
helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)