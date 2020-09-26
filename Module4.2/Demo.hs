module Demo where

data Point = Point Double Double deriving Show

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

data Roots = Roots Double Double | None 
    deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0 = Roots x1 x2
    | otherwise  = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c

data Shape = Circle Double | Rectangle Double Double
    deriving Show

square :: Double -> Shape
square a = Rectangle a a

area :: Shape -> Double
area (Circle r)      = pi * r ^ 2
area (Rectangle a b) = a * b

data Result = Fail | Success

data SomeData = Correct | Malformed

doSomeWork :: SomeData -> (Result, Int)
doSomeWork Correct   = (Success, 0)
doSomeWork Malformed = (Fail, 1)


data Result' = Fail' Int | Success'

instance Show Result' where
    show Success'  = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' someData = 
    case doSomeWork someData of
        (Success,0) -> Success'
        (Fail,n)    -> Fail' n

processData :: SomeData -> String
processData someData =
    case doSomeWork someData of
        (Success,0) -> "Success"
        (Fail,n)    -> "Fail: " ++ (show n)

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _               = False

data Bit = Zero | One deriving (Show,Eq)
data Sign = Minus | Plus deriving (Show,Eq)
data Z = Z Sign [Bit] deriving (Show,Eq)

-- instance Show Bit where
--     show Zero = "0"
--     show One  = "1"

-- instance Show Sign where
--     show Minus = "-"
--     show Plus  = "+"

-- instance Show Z where
--     show (Z sign bits) = (show sign) ++ (concatMap show . reverse $ bits) 

class ToInt a where
    toInt :: a -> Int

instance ToInt Sign where
    toInt Minus = (-1)
    toInt Plus  = 1

instance ToInt Bit where
    toInt Zero = 0
    toInt One  = 1

instance ToInt Z where
    toInt (Z sign bits) = (toInt sign) * (sum $ zipWith (*) (map toInt bits) (map (2^) [0..]))

class FromInt a where
    fromInt :: Int -> a
    
instance FromInt Sign where
    fromInt x | x < 0     = Minus
              | otherwise = Plus

instance FromInt Bit where
    fromInt 0 = Zero
    fromInt 1 = One
    fromInt _ = error "Arg should be 0 or 1."

instance FromInt Z where
    fromInt x = Z (fromInt x) (helper . abs $ x)
        where
            helper :: Int -> [Bit]
            helper x | x < 0 = error "Arg should be non negative."
            helper 0 = [Zero]
            helper 1 = [One]
            helper x = fromInt (x `mod` 2) : helper (x `div` 2)

add :: Z -> Z -> Z
add x y = fromInt (toInt x + toInt y)

mul :: Z -> Z -> Z
mul x y = fromInt (toInt x * toInt y)


emptyZ  = Z Plus []

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testAdd = putStr $ foldr (++) "" $ zipWith (\str res -> str ++ " = " ++ show res ++ "\n")
    ["test001", "test002", "test003", "test011", "test012", "test013", "test021", "test022", "test023", "test031", "test032", "test033", "test041", "test042", "test043", "test051", "test052", "test053", "test054", "test055", "test056", "test057", "test058"] 
    [test001, test002, test003, test011, test012, test013, test021, test022, test023, test031, test032, test033, test041, test042, test043, test051, test052, test053, test054, test055, test056, test057, test058]
testMul = [test101, test102, test103, test104, test105, test111, test112, test113, test114, test121, test122, test131]

--testAll = testAdd ++ testMul

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing  = error "!!!"

fromMaybe' :: Maybe a -> a
fromMaybe' ~(Just x) = x

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(***) f g ~(x,y) = (f x, g y)