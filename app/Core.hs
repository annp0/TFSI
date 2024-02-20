module Core where
-- Core Semantics

class Core exp where
    -- zero-indexed variables
    z :: exp (a, h) a
    -- succ-indexed variables
    s :: exp h a -> exp (b, h) a
    -- lambda abstraction
    l :: exp (a, h) b -> exp h (a -> b)
    -- lambda application
    a :: exp h (a -> b) -> exp h a -> exp h b

    -- natural numbers
    int :: Int -> exp h Int
    add :: exp h Int -> exp h Int -> exp h Int
    neg :: exp h Int -> exp h Int
    mul :: exp h Int -> exp h Int -> exp h Int

newtype Exp h a = Exp { ev :: h -> a }

instance Core Exp where
    z = Exp $ \(a, h) -> a
    s e = Exp $ \(b, h) -> (ev e) h
    l e = Exp $ \h -> \a -> ev e (a, h)
    a e1 e2 = Exp $ \h -> (ev e1 h) (ev e2 h)
    int i = Exp $ \h -> i
    add i1 i2 = Exp $ \h -> (ev i1 h) + (ev i2 h)
    neg i = Exp $ \h -> - (ev i h)
    mul i1 i2 = Exp $ \h -> (ev i1 h) * (ev i2 h)

eval :: Exp () a -> a
eval e = ev e () 

newtype Str h a = Str { str :: Int -> String }
-- to form Str h a, one need to supply:
-- Str $ \h -> t, where h is of type Int, and t is of type String

instance Core Str where
    z = Str $ \h -> "x" ++ show (h - 1)
    s e = Str $ \h -> (str e) (h - 1)
    l e = Str $ \h -> "(l x" ++ show h ++ " -> " ++ (str e) (h + 1) ++ " )"
    a e1 e2 = Str $ \h -> "(a " ++ (str e1 h) ++ " " ++ (str e2 h) ++ " )"
    int i = Str $ \h -> show i
    add i1 i2 = Str $ \h -> "(add " ++ (str i1 h) ++ " " ++ (str i2 h) ++ " )"
    neg i = Str $ \h -> "(neg " ++ (str i h) ++ " )"
    mul i1 i2 = Str $ \h -> "(mul " ++ (str i1 h) ++ " " ++ (str i2 h) ++ " )"

view :: Str () a -> String
view s = str s 0