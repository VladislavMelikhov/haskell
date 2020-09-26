module Demo where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab arg | enrageGork && enrageMork = stomp $ stab arg
                    | enrageMork               = stomp arg
                    | enrageGork               = stab arg
                    | otherwise                = arg
        where 
            enrageGork = doesEnrageGork arg
            enrageMork = doesEnrageMork arg

a = 127.2
b = 24.1
c = 20.1
d = 2
ip = show a ++ show b ++ show c ++ show d

--class Enum a where
--    succ, pred :: a -> a
--    toEnum :: Int -> a
--    fromEnum :: a -> Int

--class Bounded a where
--    minBound, maxBound :: a

class (Eq a, Enum a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    spred :: a -> a
    spred arg = if (arg == minBound) then maxBound else pred arg
    ssucc arg = if (arg == maxBound) then minBound else succ arg

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger (toInteger x + toInteger y + toInteger z) / 3.0
