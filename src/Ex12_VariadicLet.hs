{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex12_VariadicLet where

import Prelude hiding (and, or, not)
import Data.Functor.Identity
import Ex03_Language
import Ex09_HListRepr
import Ex11_MutuallyRecursiveLetBindings


------------------------------------------------------------
-- Splice - Apply variadic function on HList elements

class HSplice repr ys r f | f -> repr ys r where
    hSplice :: f -> HList repr ys -> Identity (repr r)

instance ( ys ~ '[] )
      => HSplice repr ys r (Identity (repr r)) where
    hSplice r HNil = r

instance ( ys ~ (y ': ys')
         , HSplice repr ys' r f' )
      => HSplice repr ys r (repr y -> f') where
    hSplice f (y :. ys) = hSplice (f y) ys


------------------------------------------------------------
-- Construct LetBindings from variadic functions.

class VLet (repr :: * -> *) (ys :: [*]) (zs :: [*]) h r | r -> repr ys zs h where
    vlet' :: h -> r

vlet :: VLet repr ys ys (LetBindings repr ys -> LetBindings repr ys) r
     => r
vlet = vlet' id

in_ :: (LetLang repr, HSplice repr ys r e)
    => LetBindings repr ys -> e -> repr r
in_ x e = let_ x (runIdentity . hSplice e)

instance ( zs ~ '[]
         , h ~ (LetBindings' repr ys '[] -> LetBindings repr ys) )
      => VLet repr ys zs h (LetBindings repr ys) where
    vlet' h = h End

instance ( zs ~ (z ': zs')
         , h ~ (LetBindings' repr ys zs -> LetBindings repr ys)
         , h' ~ (LetBindings' repr ys zs' -> LetBindings repr ys)
         , HSplice repr ys z x
         , VLet repr ys zs' h' r )
      => VLet repr ys zs h (x -> r) where
    vlet' h x = vlet' (h . (runIdentity . hSplice x :&))


------------------------------------------------------------
-- Example usage

-- does not compile
-- false = vlet (\ev od ->
--                lam (\n -> (n `eq` int 0) `or`
--                           ap od (n `sub` int 1)))
--              (\ev od ->
--                lam (\n -> (n `neq` int 0) `and`
--                           ap ev (n `sub` int 1)))
--     `in_` ( \ev od ->
--             lam (\n -> ap ev n `and` ap od n) )
