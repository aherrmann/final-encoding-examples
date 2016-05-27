{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex03_Language where

import qualified Prelude as P
import Prelude hiding (and, or, not)
import Control.Applicative

------------------------------------------------------------
-- Language definition

class AddLang repr where
    int :: Int -> repr Int
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    sub :: repr Int -> repr Int -> repr Int

class MulLang repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolLang repr where
    bool :: Bool -> repr Bool
    not :: repr Bool -> repr Bool
    and :: repr Bool -> repr Bool -> repr Bool
    or :: repr Bool -> repr Bool -> repr Bool

class EqLang repr where
    eq :: Eq a => repr a -> repr a -> repr Bool
    neq :: Eq a => repr a -> repr a -> repr Bool

class IfLang repr where
    if_ :: repr Bool -> repr a -> repr a -> repr a

class LambdaLang repr where
    lam :: (repr a -> repr b) -> repr (a -> b)
    ap :: repr (a -> b) -> repr a -> repr b


------------------------------------------------------------
-- Language evaluation

newtype Eval a = Eval { eval :: a }

instance Functor Eval where
    fmap f x = Eval $ f (eval x)

instance Applicative Eval where
    pure = Eval
    f <*> x = Eval $ (eval f) (eval x)

instance AddLang Eval where
    int = pure
    neg = liftA negate
    add = liftA2 (+)
    sub = liftA2 (-)

instance MulLang Eval where
    mul = liftA2 (*)

instance BoolLang Eval where
    bool = pure
    not = liftA P.not
    and = liftA2 (&&)
    or = liftA2 (||)

instance EqLang Eval where
    eq = liftA2 (==)
    neq = liftA2 (/=)

instance IfLang Eval where
    if_ cond then_ else_ = Eval $ if (eval cond)
                                    then (eval then_)
                                    else (eval else_)

instance LambdaLang Eval where
    lam f = Eval $ eval . f . Eval
    ap = (<*>)


------------------------------------------------------------
-- Pretty printing

data Pretty a = Pretty { unPretty :: Int -> ShowS }

lit v = Pretty $ \_ s -> shows v s
uniOp op x = Pretty $ \h s -> op ++ unPretty x h s
binOp op a b = Pretty $ \h s ->
    '(' : unPretty a h (op ++ unPretty b h (')' : s))

pretty :: Pretty a -> String
pretty e = unPretty e 0 ""

instance AddLang Pretty where
    int = lit
    neg = uniOp "-"
    add = binOp " + "
    sub = binOp " - "

instance MulLang Pretty where
    mul = binOp " * "

instance BoolLang Pretty where
    bool = lit
    not = uniOp "not "
    and = binOp " && "
    or  = binOp " || "

instance EqLang Pretty where
    eq  = binOp " == "
    neq = binOp " /= "

instance IfLang Pretty where
    if_ cond then_ else_ = Pretty $ \h s ->
        "(if " ++ unPretty cond h
        (" then " ++ unPretty then_ h
        (" else " ++ unPretty else_ h
        (')' : s)))

instance LambdaLang Pretty where
    lam f = Pretty $ \h s ->
        let x = Pretty $ \_ s' -> 'x' : shows h s'
         in "(\\" ++ unPretty x 0 (" -> " ++
             unPretty (f x) (succ h) (')' : s))
    ap f x = Pretty $ \h s ->
        '(' : unPretty f h (' ' : unPretty x h (')' : s))


------------------------------------------------------------
-- Example function

fun = lam (\n -> (n `mul` n) `eq` (int 5 `mul` int 5))

pyth = lam (\a -> lam (\b -> lam (\c ->
    (a `mul` a) `add` (b `mul` b) `eq` (c `mul` c)
    )))

true = pyth `ap` int 3 `ap` int 4 `ap` int 5
