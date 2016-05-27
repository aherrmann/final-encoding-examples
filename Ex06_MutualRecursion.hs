module Ex06_MutualRecursion where

import Prelude hiding (even, odd)
import Ex04_HaskellFactorial

even n = n == 0 || odd (n-1)
odd  n = n /= 0 && even (n-1)

evodH1 (ev, od) = ( \n -> n == 0 || od (n-1)
                  , \n -> n /= 0 && ev (n-1) )

evodH2 = ( \(ev,od) -> \n -> n == 0 || od (n-1)
         , \(ev,od) -> \n -> n /= 0 && ev (n-1) )

fixH2 :: ( (a, b) -> a
         , (a, b) -> b )
      -> (a, b)
fixH2 fs@(a, b) = (a (fixH2 fs), b (fixH2 fs))

fixL :: [[a] -> a] -> [a]
fixL fs = map (\f -> f (fixL fs)) fs

evodL = [ \[ev,od] n -> n == 0 || od (n-1)
        , \[ev,od] n -> n /= 0 && ev (n-1) ]

anInt = if aBool then 1 else 0
aBool = anInt == 0
