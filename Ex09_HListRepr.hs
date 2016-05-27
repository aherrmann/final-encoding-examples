{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex09_HListRepr where

import Ex03_Language

data family HList :: (* -> *) -> [*] -> *
data instance HList repr '[] = HNil
data instance HList repr (x ': xs) = repr x :. HList repr xs
infixr 5 :.

-- type can be inferred (constraints omitted)
-- threeThings :: HList repr '[Int, Bool, (Int -> Bool)]
threeThings = int 4 :. bool True :. lam (\n -> n `eq` int 3) :. HNil
