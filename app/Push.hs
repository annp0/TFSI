module Push where
import Interpreter

data Ctx = Pos | Neg

instance (AbsExp exp) => AbsExp (Ctx -> exp) where
    lit n Pos = lit n
    lit n Neg = neg (lit n)
    neg e Pos = e Neg
    neg e Neg = e Pos
    add e1 e2 ctx = add (e1 ctx) (e2 ctx)

pushNeg :: (Ctx -> t) -> t
pushNeg e = e Pos