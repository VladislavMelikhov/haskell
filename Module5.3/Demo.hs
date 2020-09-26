module Demo where

newtype Identity a = Identity { runIdentity :: a }
    deriving (Show, Eq)

instance Monad Identity where
    return x = Identity x
    Identity x >>= k = k x

wrap'n'succ  :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

instance Functor Identity where
    fmap f x = x >>= return . f

{- первый закон монад
return a >>= k === k a
-}

{- второй закон монад
m >>= return === m
-}

{-
return :: (Monad m) => a -> m a
f :: (Monad m) => a -> m b
g :: (Monad m) => b -> m c
h :: (Monad m) => c -> m d
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)

Левая единица
return >=> f === f 

Правая единица
f >=> return === f

Ассоциативность
f >=> (g >=> h) === (f >=> g) >=> h
-}

{- третий закон монад
m >>= k >>= k' === m >>= (\x -> k x >>= k')
-}

goWrap0 = 
    wrap'n'succ 3 >>=
    wrap'n'succ >>=
    wrap'n'succ >>=
    return

goWrap1 = 
    wrap'n'succ 3 >>= (\x ->
    wrap'n'succ x >>= (\y ->
    wrap'n'succ y >>= (\z ->
    return z)))
    
goWrap2 =
    wrap'n'succ 3 >>= (\x -> -- x := succ 3;
    wrap'n'succ x >>= (\y -> -- y := succ x;
    wrap'n'succ y >>= (\z -> -- z := succ y;
    return (x,y,z))))        -- return (x,y,z)
    
goWrap3 =
    wrap'n'succ 3 >>= (\x -> -- x := succ 3;
    wrap'n'succ x >>= (\y -> -- y := succ x;
    wrap'n'succ y >>
    return (x + y)))         -- return (x + y)

{-
do { e1; e2 }           === e1 >> e2
do { p <- e1; e2 }      === e1 >>= \p -> e2
Если p - образец (например (Just x)), и сопоставлением с ним происходит неудачно, будет вызвана функция fail
do { let v = e1; e2 }   === let v = e1 in do e2
-}

goWrap4 =
    let i = 3 in
    wrap'n'succ i >>= (\x -> 
    wrap'n'succ x >>= (\y ->
    wrap'n'succ y >>
    return (i, x + y)))         

goWrap5 = do
    let i = 3
    x <- wrap'n'succ i 
    y <- wrap'n'succ x
    wrap'n'succ y
    return (i, x + y)         
    
    