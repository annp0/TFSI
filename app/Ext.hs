module Ext where
-- Extensions of the core semantics

import Core

class ExtBool exp where
    bool :: Bool -> exp h Bool
    lseq :: exp h Int -> exp h Int -> exp h Bool
    eq :: exp h Int -> exp h Int -> exp h Bool
    ifel :: exp h Bool -> exp h a -> exp h a -> exp h a

instance ExtBool Exp where
    bool b = Exp $ \h -> b
    lseq e1 e2 = Exp $ \h -> ((ev e1 h) <= (ev e2 h))
    eq e1 e2 = Exp $ \h -> ((ev e1 h) == (ev e2 h))
    ifel e1 e2 e3 = Exp $ \h -> if (ev e1 h) then (ev e2 h) else (ev e3 h)

instance ExtBool Str where
    bool b = Str $ \h -> show b
    lseq e1 e2 = Str $ \h -> "( " ++ str e1 h ++ " <= " ++ str e2 h ++ " )"
    eq e1 e2 = Str $ \h -> "( " ++ str e1 h ++ " == " ++ str e2 h ++ " )"
    ifel e1 e2 e3 = Str $ \h -> "(if " ++ str e1 h ++ " then " ++ str e2 h ++ " else " ++ str e3 h ++ " )"

class ExtFix exp where
    fix :: exp h (a -> a) -> exp h a

instance ExtFix Exp where
    fix e = Exp $ \h -> (ev e h) (ev (fix e) h)

instance ExtFix Str where
    fix e = Str $ \h -> "(fix " ++ str e h ++ " )" 