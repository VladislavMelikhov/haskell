module Demo where

import Data.Time.Clock
import Data.Time.Format
import System.Locale

data Person' = Person' String String Int

firstName' :: Person' -> String
firstName' (Person' x _ _) = x

lastName' :: Person' -> String
lastName' (Person' _ y _) = y

age' :: Person' -> Int
age' (Person' _ _ z) = z

data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show,Eq)

--data Student = Student { firstName :: String }

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x


timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString logEntry = (timeToString . timestamp $ logEntry) ++ ": " ++ (show . logLevel $ logEntry) ++ ": " ++ message logEntry

updateAge :: Int -> Person -> Person
updateAge newAge person = person { age = newAge }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 { lastName = lastName person1 } 

name :: Person -> String
name person = firstName person ++ " " ++ lastName person

name' :: Person -> String
name' (Person fn ln _) = fn ++ " " ++ ln

name'' :: Person -> String
name'' (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln

abbrFirstName :: Person -> Person
abbrFirstName person = person { firstName = shortName . firstName $ person }
    where 
        shortName :: String -> String
        shortName (x:_:_) = [x] ++ "."
        shortName xs      = xs