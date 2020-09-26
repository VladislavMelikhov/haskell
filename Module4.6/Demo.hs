module Demo where

import Data.Char
import qualified Data.Map as Map
import Prelude hiding (lookup)
import qualified Data.List as L

--type String = [Char]

allUpper :: String -> Bool
allUpper = all isUpper

type IntegerList = [Integer]

sumSquares :: IntegerList -> Integer
sumSquares = foldr1 (+) . map (^2)

type AssocList k v = [(k,v)]

lookup' :: Eq k => k -> AssocList k v -> Maybe v
lookup' _ []            = Nothing
lookup' key ((x,y):xys) 
    | key == x          = Just y
    | otherwise         = lookup' key xys

type IntMap = Map.Map Int

newtype IntList = IList [Int] deriving Show

example = IList [1,2]

data IntList' = IList' [Int] deriving Show

ignore' :: IntList' -> String
ignore' (IList' _) = "Hello"

ignore :: IntList -> String
ignore (IList _) = "Hello"

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)


class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid [a] where
    mempty = []
    mappend = (++)


newtype Sum a = Sum {getSum :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product {getProduct :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq, Show)

instance Monoid Xor where
    mempty = Xor False
    Xor x `mappend` Xor y = Xor (x /= y)

instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)
    (x1,x2) `mappend` (y1,y2) = (x1 `mappend` y1, x2 `mappend` y2)

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First Nothing `mappend` r = r
    l `mappend` _             = l

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    Maybe' Nothing `mappend` m = Maybe' Nothing
    m `mappend` Maybe' Nothing = Maybe' Nothing
    Maybe' (Just m1) `mappend` Maybe' (Just m2) = Maybe' (Just (m1 `mappend` m2))

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }    
    deriving (Eq, Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap xs) = lookupHelper k xs
        where
            lookupHelper _ [] = Nothing
            lookupHelper k ((x,y):xys) | k == x = Just y
                                       | otherwise = lookupHelper k xys
    insert k v (ListMap xs) = ListMap (insertHelper k v xs)
        where
            insertHelper k v [] = [(k,v)]
            insertHelper k v ((x,y):xys) | k == x = (x,v) : xys
                                         | otherwise = (x,y) : insertHelper k v xys
    delete k (ListMap xs) = ListMap (deleteHelper k xs)
        where
            deleteHelper k [] = []
            deleteHelper k ((x,y):xys) | k == x = deleteHelper k xys
                                       | otherwise = (x,y) : deleteHelper k xys

newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\x -> Nothing)
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = (ArrowMap g)
        where
            g key | key == k  = Just v
                  | otherwise = f key
    delete k (ArrowMap f) = (ArrowMap g)
        where
            g key | key == k  = Nothing
                  | otherwise = f key
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)