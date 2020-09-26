module Main where

import Control.Monad
import System.Directory (getDirectoryContents, removeFile)
import Data.List (isInfixOf)

main :: IO ()
main = main'

main' :: IO ()
main' = do
    putStr "Substring: "
    name <- getLine
    if name == "" then
        putStrLn "Canceled"
    else do
        files <- (getDirectoryContents ".")
        let filtered = filter (isInfixOf name) files
        mapM_ removeFile filtered
        mapM_ putStrLn . zipWith (++) (repeat "Removing file: ") $ filtered

main'' :: IO ()
main'' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if name == "" then main'
                  else putStrLn $ "Hi, " ++ name ++ "!"




{-
getCharFromConsole :: Char - плохо
getCharFromConsole :: RealWorld -> (RealWorld, Char)
newtype IO a = IO (RealWorld -> (RealWorld, a))
-}

getLine' :: IO String
getLine' = do
    c <- getChar
    if c == '\n' then
        return []
    else do
        cs <- getLine'
        return (c:cs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs

{-
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f
-}

putStr''' :: String -> IO ()
putStr''' = mapM_ putChar



{-
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
    where
        k :: Monad m => m a -> m [a] -> m [a]
        k m m' = do
            x <- m
            xs <- m'
            return (x:xs)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f
-}