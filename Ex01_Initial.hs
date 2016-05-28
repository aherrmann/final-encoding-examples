module Ex01_Initial where

data Expr = Lit Int        -- literal value
          | Neg Expr       -- negation
          | Add Expr Expr  -- addition
          deriving (Eq, Show)

exprI = Add (Lit 1) (Add (Lit 2) (Neg (Lit 3)))

eval :: Expr -> Int
eval (Lit n) = n
eval (Neg x) = negate (eval x)
eval (Add a b) = (eval a) + (eval b)

prettyS :: Expr -> ShowS
prettyS (Lit n) = shows n
prettyS (Neg x) = \s -> '-' : prettyS x s
prettyS (Add a b) = \s -> '(' : prettyS a (" + " ++ prettyS b (')' : s))

pretty :: Expr -> String
pretty e = prettyS e ""
