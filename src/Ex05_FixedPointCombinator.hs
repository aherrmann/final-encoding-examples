{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex05_FixedPointCombinator where

import Ex03_Language

------------------------------------------------------------
-- Language definition

class FixLang repr where
    fix :: (repr a -> repr a) -> repr a


------------------------------------------------------------
-- Expression evaluation

instance FixLang Eval where
    fix f = f (fix f)


------------------------------------------------------------
-- Pretty printing

instance FixLang Pretty where
    fix f = Pretty $ \h s ->
        let self = Pretty $ \_ s' -> "self" ++ shows h s'
         in "fix (\\" ++ unPretty self 0 (" -> " ++
             unPretty (f self) (succ h) (')' : s))


------------------------------------------------------------
-- Example function

fac = fix ( \self -> lam ( \n ->
              if_ (n `eq` (int 0))
                  (int 1)
                  (n `mul` (ap self (n `sub` (int 1))))
            )
          ) 
