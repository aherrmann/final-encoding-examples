module Ex04_HaskellFactorial where

facH n = if n == 0 then 1 else n * facH (n-1)

open_fac self n = if n == 0 then 1 else n * self (n-1)

fixH :: (a -> a) -> a
fixH f = f (fixH f)

facH2 = fixH open_fac
