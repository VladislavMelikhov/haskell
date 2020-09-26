module Demo where

import Prelude hiding (Bool,True,False)

data Bool = True | False

alwaysTrue :: Int -> Bool
alwaysTrue n = True

data B = T | F deriving (Show,Eq,Read,Enum)

not' :: B -> B
not' F = T
not' T = F

data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue


intToChar :: Int -> Char
intToChar 1 = '1'
intToChar 2 = '2'
intToChar 3 = '3'
intToChar 4 = '4'
intToChar 5 = '5'
intToChar 6 = '6'
intToChar 7 = '7'
intToChar 8 = '8'
intToChar 9 = '9'
intToChar _ = 'N'

isz :: Char -> Bool
isz 'z' = True
isz _   = False

stringToBool :: String -> Bool
stringToBool "true"  = True
stringToBool "false" = False
stringToBool _       = error "Malformed parameter"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

foo 1 2 = 3
foo 0 x = 5

bar (1,2) = 3
bar (0,_) = 5

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp first second = compare (priority first) (priority second)
    where
         priority :: LogLevel -> Int
         priority Error   = 3
         priority Warning = 2
         priority Info    = 1

lessThanError :: LogLevel -> Bool
lessThanError lvl =
    case cmp lvl Error of
        LT -> True
        _  -> False

data Result = Fail | Success

data SomeData = Correct | Malformed

doSomeWork :: SomeData -> (Result, Int)
doSomeWork Correct   = (Success, 0)
doSomeWork Malformed = (Fail, 1)

processData :: SomeData -> String
processData someData =
    case doSomeWork someData of
        (Success,0) -> "Success"
        (Fail,n)    -> "Fail: " ++ (show n)
