module Demo where

safeHead = do
    b <- null
    if b
    then return Nothing
    else do
        h <- head
        return $ Just h

safeHead' = do
    e <- id
    if null e
    then return Nothing
    else return $ Just (head e)

{-
return 2 >>= (+) >>= (*)
\e -> 2 >>= (+) >>= (*)
(*) ((+) ((\e -> 2) e) e) e 
-}

newtype Reader r a = Reader { runReader :: (r -> a) }

--runReader :: Reader r a -> r -> a

instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    m >>= k  = Reader $ \e ->
        let v = runReader m e
        in runReader (k v) e

ask :: Reader r r
ask = Reader id

type User = String
type Password = String
type UsersTable = [(User,Password)]

pwds :: UsersTable
pwds = [("Bill","123"),("Ann","qwerty"),("John","2sRs8p")]

firstUser :: Reader UsersTable User
firstUser = do
    e <- ask
    return $ fst (head e)

asks :: (r -> a) -> Reader r a
asks = Reader

firstUserPwd :: Reader UsersTable Password
--firstUserPwd = asks (snd . head)
firstUserPwd = do
    pwd <- asks (snd . head)
    return pwd

usersCount :: Reader UsersTable Int
usersCount = asks length

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

localTest :: Reader UsersTable (Int,Int)
localTest = do
    count1 <- usersCount
    count2 <- local (("Mike","1"):) usersCount
    return (count1, count2)

local' :: (r' -> r) -> Reader r a -> Reader r' a
local' f m = Reader $ \e -> runReader m (f e)

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ map fst . filter ((=="123456") . snd)

{-
usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ \xs -> [x | (x, "123456") <- xs]
-}