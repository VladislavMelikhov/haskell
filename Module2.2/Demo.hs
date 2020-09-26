module Demo where

import Data.Function

sumFstFst = (+) `on` (fst . fst)

doItYourself = f . g . h

f = logBase 2

g = (^3)

h = max 42

avg :: (Double, Double) -> Double
avg p = (fst p + snd p) / 2