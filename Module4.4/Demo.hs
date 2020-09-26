module Demo where

import Data.Char(isDigit)
import Text.Read(readMaybe)


data CoordD = CoordD Double Double

data CoordI = CoordI Int Int

data Coord a = Coord a a deriving(Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

getCenter :: Double -> Coord Int -> Coord Double
getCenter step (Coord x y) = Coord (helper x) (helper y)
    where 
        helper :: Int -> Double
        helper x = (fromIntegral x + 0.5) * step

getCell :: Double -> Coord Double -> Coord Int
getCell step (Coord x y) = Coord (modDouble x step) (modDouble y step)
    where
        modDouble :: Double -> Double -> Int
        modDouble a b | a > 0     = modDoublePositive a b 0
                      | otherwise = modDoubleNegative a b 0
        modDoublePositive :: Double -> Double -> Int -> Int
        modDoublePositive a b n | a < b     = n
                                | otherwise = modDoublePositive (a - b) b (n + 1)
                                
        modDoubleNegative :: Double -> Double -> Int -> Int
        modDoubleNegative a b n | a >= (-b) = n - 1
                                | otherwise = modDoubleNegative (a + b) b (n - 1)

----------------------------------------
change :: (a -> b) -> Coord a -> Coord b
change f (Coord a b) = Coord (f a) (f b)

getCenter' :: Double -> Coord Int -> Coord Double
getCenter' step = change $ (step *) . (+ 0.5) . fromIntegral

getCell' :: Double -> Coord Double -> Coord Int
getCell' step = change $ floor . (/step)

twice :: a -> [] a
twice x = [x,x]

thrice :: a -> (,,) a a a
thrice x = (,,) x x x

id' :: (->) a a
id' x = x

k :: (->) a ((->) b a)
k x y = x

roots :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots a b c
    | discr >= 0 = Right (x1, x2)
    | otherwise  = Left "Negative discriminant"
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Nothing  -> 'X'
    (Just x) -> x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving(Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving(Show)

parsePerson :: String -> Either Error Person
parsePerson = toPerson . toTriple . toEntryList . map toEntry . split '\n'

toPerson :: Either Error (String, String, String) -> Either Error Person
toPerson (Left error)               = Left error
toPerson (Right (first, second, third)) = case readMaybe third of
    Nothing      -> Left (IncorrectDataError third)
    (Just value) -> Right Person { firstName = first, lastName = second, age = value }

toTriple :: Either Error [(String, String)] -> Either Error (String, String, String)
toTriple (Left error)    = Left error
toTriple (Right entries) = helper firstName lastName age
    where
        helper :: Maybe String -> Maybe String -> Maybe String -> Either Error (String, String, String)
        helper (Just first) (Just second) (Just third) = Right (first, second, third)
        helper _ _ _                                   = Left IncompleteDataError
        firstName = valueOf entries "firstName"
        lastName = valueOf entries "lastName"
        age = valueOf entries "age"

valueOf :: [(String, String)] -> String -> Maybe String
valueOf []               _   = Nothing
valueOf ((key,value):xs) arg | arg == key   = Just value
                             | otherwise    = valueOf xs arg

toEntryList :: [Either Error (String, String)] -> Either Error [(String, String)]
toEntryList entries = helper entries []
    where
        helper :: [Either Error (String, String)] -> [(String, String)] -> Either Error [(String, String)]
        helper [] results                      = Right results
        helper ((Right entry):entries) results = helper entries (entry:results)
        helper ((Left error):_) _              = Left error

toEntry :: String -> Either Error (String, String)
toEntry str = case split '=' str of
    (x1:x2:_) -> convert x1 x2
    _         -> Left ParsingError
    where
        convert :: String -> String -> Either Error (String, String)
        convert first second = case dropSpaces first second of
            (Just x, Just y) -> Right (x, y)
            _                -> Left ParsingError
        dropSpaces :: String -> String -> (Maybe String, Maybe String) 
        dropSpaces first second = (dropLastSpace first, dropFirstSpace second)
        dropLastSpace :: String -> Maybe String
        dropLastSpace []  = Nothing
        dropLastSpace [x] | x == ' '  = Just []
                          | otherwise = Nothing
        dropLastSpace (x:xs) = case dropLastSpace xs of
            Nothing   -> Nothing
            (Just ys) -> Just (x:ys)
        dropFirstSpace :: String -> Maybe String
        dropFirstSpace [] = Nothing
        dropFirstSpace (x:xs) | x == ' '  = Just xs
                              | otherwise = Nothing

split :: Char -> String -> [String]
split delim []     = [[]]
split delim (x:xs) = let ys'@(y:ys) = split delim xs in
    if x == delim then ([]:ys') else ((x:y):ys)

data CoordLazy a = CoordLazy a a
    deriving(Show)

data CoordStrict a = CoordStrict !a !a
    deriving(Show)

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x

{-
data Complex a = !a :+ !a
data Ratio a   = !a :% !a
-}