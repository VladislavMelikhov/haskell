module Demo where

import Data.Function(on)

data List a = Nil | Cons a (List a)
    deriving Show
{-
data [] a = [] | a : ([] a)
-}

fromList :: List a -> [a]
fromList Nil         = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil

{-
instance Foldable List where
   foldMap f Nil = mempty
   foldMap f (Cons x y) = f x `mappend` foldMap f y

fromList :: List a -> [a]
fromList = foldr (:) []
-}


data Nat = Zero | Suc Nat deriving(Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero ys     = ys
add (Suc xs) ys = add xs (Suc ys)

mul :: Nat -> Nat -> Nat
mul xs ys = mulHelper xs ys Zero
    where
        mulHelper :: Nat -> Nat -> Nat -> Nat
        mulHelper Zero ys res     = res
        mulHelper (Suc xs) ys res = mulHelper xs ys (add ys res)

fac :: Nat -> Nat
fac xs = facHelper xs (Suc Zero)
    where
        facHelper :: Nat -> Nat -> Nat
        facHelper Zero res         = res
        facHelper xs'@(Suc xs) res = facHelper xs (mul res xs')

data Tree a = Leaf a | Node (Tree a) (Tree a)


height :: Tree a -> Int
height (Leaf a)          = 0
height (Node left right) = 1 + (max `on` height) left right

size :: Tree a -> Int
size (Leaf a)          = 1
size (Node left right) = 1 + ((+) `on` size) left right

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int) 
    go (Leaf x)          = (1,x)
    go (Node left right) = (sum `on` go) left right
    sum :: (Int,Int) -> (Int,Int) -> (Int,Int)
    sum (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show,Eq)

expr1 = Val 2 :+: Val 3 :*: Val 4
expr2 = (Val 2 :+: Val 3) :*: Val 4

eval :: Expr -> Int
eval (Val n)     = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

expand :: Expr -> Expr
expand e = let expanded = expand' e in if e == expanded then expanded else expand expanded
    where
        expand' ((e1 :+: e2) :*: e) = expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
        expand' (e :*: (e1 :+: e2)) = expand' e :*: expand' e1 :+: expand' e :*: expand' e2
        expand' (e1 :+: e2)         = expand' e1 :+: expand' e2
        expand' (e1 :*: e2)         = expand' e1 :*: expand' e2
        expand' e                   = e

foo :: Expr -> String
foo ((e1 :+: e2) :*: e) = "Ok" ++ show e1 ++ ""
_                       = "Not ok"

---------------------------------------------------------
expand :: Expr -> Expr
expand = foldr1 (:+:) . expandList
    where
        expandList :: Expr -> [Expr]
        expandList (Val i) = [Val i]
        expandList (l :+: r) = expandList l ++ expandList r
        expandList (l :*: r) = [ e1 :*: e2 | e1 <- expandList l, e2 <- expandList r]
