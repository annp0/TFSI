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

newtype Hsk h a = Hsk { hsk :: Int -> String }
-- to form Str h a, one need to supply:
-- Str $ \h -> t, where h is of type Int, and t is of type String

instance Core Hsk where
    z = Hsk $ \h -> "x" ++ show (h - 1)
    s e = Hsk $ \h -> (hsk e) (h - 1)
    l e = Hsk $ \h -> "(\\ x" ++ show h ++ " -> " ++ (hsk e) (h + 1) ++ " )"
    a e1 e2 = Hsk $ \h -> "( " ++ (hsk e1 h) ++ " " ++ (hsk e2 h) ++ " )"
    int i = Hsk $ \h -> "(" ++ show i ++ ")"
    add i1 i2 = Hsk $ \h -> "( " ++ (hsk i1 h) ++ " + " ++ (hsk i2 h) ++ " )"
    neg i = Hsk $ \h -> "(- " ++ (hsk i h) ++ " )"
    mul i1 i2 = Hsk $ \h -> "( " ++ (hsk i1 h) ++ " * " ++ (hsk i2 h) ++ " )"

toHsk :: Hsk () a -> String
toHsk e = hsk e 0

newtype Len h a = Len { len :: Int }

instance Core Len where
    z = Len 1
    s e = Len $ 1 + len e
    l e = Len $ 1 + len e
    a e1 e2 = Len $ 1 + len e1 + len e2
    int i = Len $ 1
    add i1 i2 = Len $ 1 + len i1 + len i2
    neg i = Len $ len i
    mul i1 i2 = Len $ len i1 + len i2

