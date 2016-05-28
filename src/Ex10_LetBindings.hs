{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex10_LetBindings where

import Ex03_Language

class Let1Lang repr where
    let1 :: repr a -> (repr a -> repr b) -> repr b

instance Let1Lang Eval where
    let1 x e = e x

instance Let1Lang Pretty where
    let1 x e = Pretty $ \h s ->
        let name = Pretty $ \_ s -> 'x' : shows h s
         in "(let " ++ unPretty name 0 (" = " ++ unPretty x (succ h)
            (" in " ++ unPretty (e name) (succ h) (')' : s)))

test = let1 (lam (\n -> n `mul` n)) (\sq -> ap sq (int 4))
