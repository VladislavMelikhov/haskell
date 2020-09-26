module Demo where

import Data.Monoid

newtype State s a = State { runState :: s -> (a,s) }

--runState :: State s a -> s -> (a,s)

instance Monad (State s) where
    return a = State $ \st -> (a,st)

    m >>= k = State $ \st -> 
        let (a',st') = runState m st
            m' = k a'
        in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s
get = State $ \st -> (st,st)

put :: s -> State s ()
put st = State $ \_ -> ((),st)

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n

modify :: (s -> s) -> State s ()
--modify f = State $ \s -> ((), f s)
modify f = do
    s <- get
    put (f s)

newtype Reader r a = Reader { runReader :: (r -> a) }

--runReader :: Reader r a -> r -> a

instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    m >>= k  = Reader $ \e ->
        let v = runReader m e
        in runReader (k v) e

asks :: (r -> a) -> Reader r a
asks = Reader

readerToState :: Reader r a -> State r a
readerToState m = State $ \st -> (runReader m st, st)

--readerToState (Reader f) = State $ \x -> (f x, x)

newtype Writer w a = Writer { runWriter :: (a, w) }

--runWriter :: Writer w a -> (a, w)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k =
        let (x,u) = runWriter m
            (y,v) = runWriter $ k x
        in Writer (y, u `mappend` v)

writer :: (a, w) -> Writer w a
writer = Writer

tell :: Monoid w => w -> Writer w ()
tell w = writer ((),w)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \st ->
    let (x,u) = runWriter m
    in (x, st `mappend` u)

succ' :: Int -> Int
succ' n = execState tick n

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x


replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n


plus' :: Int -> Int -> Int
plus' n x = execState (replicateM n tick) x

{-
def fib(n):
    a = 0
    b = 1
    for i in [1..n]
        a = b
        b = a + b
    return a
-}

fibStep :: State (Integer, Integer) ()
fibStep = State $ \(x,y) -> ((), (y, x + y))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0,1)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
    deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTreeStep tree) 1

numberTreeStep :: Tree () -> State Integer (Tree Integer)
numberTreeStep (Leaf _) = do
    i <- get 
    put (i + 1) 
    return (Leaf i)

numberTreeStep (Fork left _ right) = do
    left' <- numberTreeStep left
    i <- get
    put (i + 1)
    right' <- numberTreeStep right
    return (Fork left' i right')