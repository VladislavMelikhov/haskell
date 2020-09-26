module Demo where

import Prelude hiding (Functor, fmap)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = (Point3D (f x) (f y) (f z))

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving (Show)

instance Functor GeomPrimitive where
    fmap f (Point x)         = Point (fmap f x)
    fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y) 
{-
data Tree a = Leaf a | Branch (Tree a) a (Tree a)
    deriving (Show)

testTree = Branch (Leaf 2) 3 (Leaf 4)

instance Functor Tree where
    fmap g (Leaf x)       = Leaf (g x)
    fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)
-}

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap g (Leaf x)       = Leaf (fmap g x)
    fmap g (Branch l x r) = Branch (fmap g l) (fmap g x) (fmap g r)

instance Functor ((,) s) where
    fmap g (x,y) = (x, g y)

instance Functor (Either e) where
    fmap _ (Left x)  = Left x
    fmap g (Right y) = Right (g y)

instance Functor ((->) e) where
    fmap = (.)

data Entry k1 k2 v = Entry (k1, k2) v deriving (Show)

data Map k1 k2 v = Map [Entry k1 k2 v] deriving (Show)

instance Functor (Entry k1 k2) where
    fmap g (Entry k v) = Entry k (g v)

instance Functor (Map k1 k2) where
    fmap g (Map entries) = Map (fmap (fmap g) entries)

{-
    (1) fmap id = fmap
    (2) fmap (f . g) = fmap f . fmap g
-}