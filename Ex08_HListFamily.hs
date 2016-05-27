{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ex08_HListFamily where

data family HListF :: [*] -> *
data instance HListF '[] = HNilF
data instance HListF (x ': xs) = HConsF x (HListF xs)

-- type can be inferred
-- listF :: HListF '[Char, Bool]
listF = HConsF 'a' (HConsF True HNilF)

-- simple recursion won't compile
-- hLengthF :: HListF xs -> Int
-- hLengthF HNilF = 0
-- hLengthF (HConsF _ xs) = 1 + hLengthF xs

-- need type-class instead
class HLengthF xs where
    hLengthF :: HListF xs -> Int

instance HLengthF '[] where
    hLengthF HNilF = 0

instance HLengthF xs => HLengthF (x ': xs) where
    hLengthF (HConsF x xs) = 1 + hLengthF xs

-- type can be inferred
-- mysteriousF :: HListF '[Char, Bool] -> (String, Bool)
mysteriousF = \(HConsF c (HConsF b HNilF)) -> (c:"", not b)
