module Demo where

--x :: Num Bool => Bool
--x = True + False * False

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x Demo.== y)
    x == y = not (x Demo./= y)

instance Demo.Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True  = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Demo.Eq a, Demo.Eq b) => Demo.Eq (a, b) where
    p1 == p2 = fst p1 Demo.== fst p2 && snd p1 Demo.== snd p2

instance (Printable a, Printable b) => Printable (a, b) where
    toString (first,second) = "(" ++ (toString first) ++ "," ++ (toString second) ++ ")"

class (Demo.Eq a) => Ord a where
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering