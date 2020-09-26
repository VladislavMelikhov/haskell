module Demo where


newtype IO a = IO (RealWorld -> (RealWorld, a))

type IO a = RealWorld -> (RealWorld, a)

return :: a -> IO a
return :: a -> RealWorld -> (RealWorld, a)
return a = \w -> (w,a)

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) :: (RealWorld -> (RealWorld, a)) -> (a -> RealWorld -> (RealWorld, b)) -> RealWorld -> (RealWorld, b)
(>>=) m k = \w -> case m w of (w',a) -> k a w' 