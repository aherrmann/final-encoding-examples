{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Ex07_HListGADT where

data HListG xs where
    HNilG :: HListG '[]
    HConsG :: x -> HListG xs -> HListG (x ': xs)

-- type can be inferred
-- listG :: HListG '[Char, Bool]
listG = HConsG 'a' (HConsG True HNilG)

-- type cannot be inferred
-- won't compile without type-signature
hLengthG :: HListG xs -> Int
hLengthG HNilG = 0
hLengthG (HConsG _ xs) = 1 + hLengthG xs

-- type cannot be inferred
-- won't compile without type-signature
mysteriousG :: HListG '[Char, Bool] -> (String, Bool)
mysteriousG = \(HConsG c (HConsG b HNilG)) -> (c:"", not b)
