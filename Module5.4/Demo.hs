module Demo where

import Data.Char (isDigit)
{-
import Prelude hiding (Maybe,Nothing,Just)

data Maybe a = Nothing | Just a
    deriving (Eq,Ord)

instance Monad Maybe where
    return = Just

    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing

    (Just _) >> m = m
    Nothing  >> _ = Nothing

    fail _ = Nothing
-}
type Name = String
type DataBase = [(Name,Name)]

fathers, mothers :: DataBase
fathers = [ ("Bill", "John")
          , ("Ann", "John")
          , ("John", "Peter")] 
mothers = [ ("Bill", "Jane")
          , ("Ann", "Jane")
          , ("John", "Alice")
          , ("Jane", "Dorothy")
          , ("Alice", "Mary")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

granmas :: Name -> Maybe (Name,Name)
granmas person = do
    m   <- getM person
    gmm <- getM m
    f   <- getF person
    gmf <- getM f
    return (gmm, gmf)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str | all isDigit str = Just . Number . read $ str
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken _   = Nothing

tokenize :: String -> Maybe [Token]
tokenize str = sequen . map asToken . words $ str

sequen :: Monad m => [m a] -> m [a]
sequen []     = return []
sequen (x:xs) = do
    ys <- sequen xs
    y <- x
    return (y : ys)

{-
instance Monad [] where
    return x = [x]
    xs >>= k = concat (map k xs)
    fail _ = []
-}

list = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

list' = do
    x <- [1,2,3]
    y <- [4,5,6]
    return (x,y)

list'' =
    [1,2,3] >>= (\x ->
    [4,5,6] >>= (\y ->
    return (x,y)))

data Board = Board

nextPositions :: Board -> [Board]
nextPositions board = [board]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0     = []
                        | otherwise = filter pred (nextPositionsNHelper b n)
    where
        nextPositionsNHelper b 0 = [b]
        nextPositionsNHelper b n = do 
            let boards = nextPositionsNHelper b (n - 1)
            board <- boards
            nextPositions board

lst = [(x,y) | x <- [1,2,3], y <- [1,2], x /= y]

lst' = do
    x <- [1,2,3]
    y <- [1,2]
    True <- return (x /= y)
    return (x,y)

lst'' = 
    [1,2,3]         >>= (\x ->
    [1,2]           >>= (\y ->
    return (x /= y) >>= (\b ->
    case b of True -> return (x,y)
              _    -> fail "...")))

lst''' = do
    x <- [1,2,3]
    y <- [1,2]
    if x /= y then [42] else []
    return (x,y)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    c <- [1..x]
    b <- [1..c]
    a <- [1..b]
    if a^2 + b^2 == c^2 then [1] else []
    return (a,b,c)

