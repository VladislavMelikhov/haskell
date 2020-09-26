module Demo where
{-
f :: a -> m b
-- Примеры:
f :: a -> Maybe b            -- m = Maybe
f :: a -> [] b               -- m = []
f :: a -> (Either s) b       -- m = Either s
f :: a -> ((,) s) b          -- m = ((,) s)
f :: a -> ((->) e) b         -- m = ((->) e)
f :: a -> (State s) b        -- m = State s
f :: a -> IO b               -- m = IO
-}

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger func message arg = Log [message] (func arg)


execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers arg func1 func2 = Log (messages1 ++ messages2) res2
    where
        (Log messages2 res2) = func2 res1
        (Log messages1 res1) = func1 arg

{-
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail s = error s

infixl 1 >>=
-}

toKleisli :: Monad m => (a -> b) -> a -> m b
toKleisli f x = return (f x)

returnLog :: a -> Log a
returnLog = Log []

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

bindLog :: Log a -> (a -> Log b) -> Log b
--bindLog (Log messages1 res1) func = Log (messages1 ++ messages2) res2
bindLog (Log messages1 res1) func = Log (messages2 ++ messages1 ++ messages2) res2
    where 
        (Log messages2 res2) = func res1

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList arg functions = foldl (>>=) (return arg) functions

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)


(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) f g x = g x >>= f