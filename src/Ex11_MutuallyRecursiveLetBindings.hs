{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex11_MutuallyRecursiveLetBindings where

import Prelude hiding (and, or, not)
import Ex03_Language
import Ex09_HListRepr

------------------------------------------------------------
-- List of let-bindings

data LetBindings' repr ys zs where
    End  :: LetBindings' repr ys '[] 
    (:&) :: (HList repr ys -> repr z)
         -> LetBindings' repr ys zs
         -> LetBindings' repr ys (z ': zs)
infixr 5 :&

type LetBindings repr ys = LetBindings' repr ys ys

-- fails to type-check
-- wrong = check
--     ( (\(anInt :. aBool :. HNil) -> anInt `eq` (int 0))
--    :& (\(anInt :. aBool :. HNil) -> if_ aBool (int 1) (int 0))
--    :& End )
--   where
--     -- used to enforce that ys == zs
--     check :: LetBindings repr ys -> LetBindings repr ys
--     check = id


------------------------------------------------------------
-- Language definition

class LetLang repr where
    let_ :: LetBindings repr ys -> (HList repr ys -> repr r) -> repr r


------------------------------------------------------------
-- Expression evaluation

instance LetLang Eval where
    let_ xs e = e (fx xs xs) where
        fx :: LetBindings Eval ys
           -> LetBindings' Eval ys ys'
           -> HList Eval ys'
        fx xs End = HNil
        fx xs (x :& xs') = x (fx xs xs) :. fx xs xs'


------------------------------------------------------------
-- Pretty printing

instance LetLang Pretty where
    let_ xs e = Pretty $ \h s ->
      let (args, h') = mkargs h xs
       in "(let " ++ bindings h h' args xs
          ("in " ++ unPretty (e args) h' (')':s))
      where
        mkargs :: Int
               -> LetBindings' Pretty ys ys'
               -> (HList Pretty ys', Int)
        mkargs h End = (HNil, h) 
        mkargs h (x :& xs') =
          let self = Pretty $ \_ s -> "self" ++ shows h s
              (args', h') = mkargs (succ h) xs'
           in (self :. args', h')
        bindings :: Int
                 -> Int
                 -> HList Pretty ys
                 -> LetBindings' Pretty ys ys'
                 -> ShowS
        bindings h h' args End s = s
        bindings h h' args (x :& xs') s =
            ("self" ++ shows h (" = " ++ unPretty (x args) h' ("; " ++
            bindings (succ h) h' args xs' s)))


------------------------------------------------------------
-- Example usage

fac = let_ ( (\(self :. HNil) ->
               lam ( \n -> if_ (n `eq` int 0)
                               (int 1)
                               (n `mul` (ap self (n `sub` int 1)))) )
          :& End )
        ( \(fac :. HNil) -> fac )

false = let_ ( (\(ev :. od :. HNil) ->
                 lam (\n -> (n `eq` int 0) `or`
                            ap od (n `sub` int 1)))
            :& (\(ev :. od :. HNil) ->
                 lam (\n -> (n `neq` int 0) `and`
                            ap ev (n `sub` int 1)))
            :& End )
          ( \(ev :. od :. HNil) ->
            lam (\n -> ap ev n `and` ap od n) )

fixE = let_ ( (\(fix :. HNil) ->
                lam (\f -> ap f (ap fix f)))
           :& End )
         ( \(fix :. HNil) -> fix )

fac2 = ap fixE $ lam (\fac ->
    lam (\n ->
      if_ (n `eq` (int 0))
          (int 1)
          (n `mul` ap fac (n `sub` int 1))
    ))
