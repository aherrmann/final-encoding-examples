{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex02_Final where

class AddLang repr where
    int :: Int -> repr Int
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int

exprF = add (int 1) (add (int 2) (neg (int 3)))


newtype Eval a = Eval { eval :: a }

instance AddLang Eval where
    int = Eval
    neg x = Eval $ negate (eval x)
    add a b = Eval $ (eval a) + (eval b)


data Pretty a = Pretty { unPretty :: ShowS }

pretty :: Pretty a -> String
pretty e = unPretty e ""

instance AddLang Pretty where
    int n = Pretty $ shows n
    neg x = Pretty $ \s -> '-' : unPretty x s
    add a b = Pretty $ \s ->
        '(' : unPretty a (" + " ++ unPretty b (')' : s))
