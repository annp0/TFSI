module Extension where

import qualified Interpreter as I

class AbsExpExt exp where
    mul :: exp -> exp -> exp

instance AbsExpExt Int where
    mul m n = m * n

instance AbsExpExt String where
    mul m n = "("++m++"*"++n++")"

instance AbsExpExt I.Tree where
    mul m n = I.Node "mul" [m, n]

instance (AbsExpExt exp, AbsExpExt exp') => AbsExpExt (exp, exp') where
    mul (m, n) (m', n') = (mul m m', mul n n')

fromTreeExt :: (AbsExpExt exp, I.AbsExp exp) => (I.Tree -> Either I.Err exp) -> (I.Tree -> Either I.Err exp)
fromTreeExt self (I.Node "mul" [m, n]) = mul <$> self m <*> self n
fromTreeExt self e = I.fromTreeExt self e

fromTree :: (AbsExpExt exp, I.AbsExp exp) => I.Tree -> Either I.Err exp
fromTree = I.fix fromTreeExt

