{-# LANGUAGE InstanceSigs #-}
module Typed where

data Exp = V Var | B Bool | L Exp | A Exp Exp
data Var = VZ | VS Var

ti1 = A (L (V VZ)) (B True)

data U = UB Bool | UA (U -> U)

lookp :: Var -> [U] -> U
lookp VZ (x:_) = x
lookp (VS v) (_:xs) = lookp v xs

eval :: [U] -> Exp -> U
eval env (V v) = lookp v env
eval env (B b) = UB b
eval env (L e) = UA (\x -> eval (x:env) e)
eval env (A e1 e2) = case eval env e1 of
                        UA f -> f (eval env e2)

-- varzero

vz :: (a, b) -> a
vz (vc, _) = vc

-- varsucc

vs :: (t1 -> t2) -> (a, t1) -> t2
vs vp (_, envr) = vp envr

-- takes variable, an environment and returns itself

b :: t -> env -> t
b bv env = bv

-- an expression is a function (t1, env) -> t

l :: ((t1, env) -> t) -> env -> t1 -> t
l e env = \x -> e (x, env)

a :: (env -> t1 -> t) -> (env -> t1) -> env -> t
a e1 e2 env = (e1 env) (e2 env)

tf1 = a (l vz) (b True) ()

class Sym repr where
    -- expressions of the language are of type repr - -
    -- repr h t represents the expression is of type t under environment h 

    -- integer literals
    int :: Int -> repr h Int
    add :: repr h Int -> repr h Int -> repr h Int
    -- zero index variables
    z :: repr (a, h) a
    s :: repr h a -> repr (any, h) a
    lam :: repr (a, h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

newtype R h a = R {unR :: h -> a}

instance Sym R where
    int :: Int -> R h Int
    int x = R $ const x
    add :: R h Int -> R h Int -> R h Int
    add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
    z :: R (a, h) a
    z = R $ \(x, _) -> x
    s :: R h a -> R (any, h) a
    s e = R $ \(any, h) -> unR e h
    lam :: R (a, h) b -> R h (a -> b)
    lam e = R $ \h -> (\a -> (unR e (a,h)))
    app :: R h (a -> b) -> R h a -> R h b
    app e ea = R $ \h -> (unR e h) (unR ea h)


evalN :: R () a -> a
evalN e = unR e ()

td1 :: R () Int
td1 = add (int 1) (int 2)

td2o :: R (Int, h) (Int -> Int)
td2o = lam (add z (s z))

td3 :: R () ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))

w :: R h Int 
w = int 1

w3 :: R (a, h) a
w3 = z

w1 :: R (Int -> a, h) a 
-- z is an expression (Int -> ?)
w1 = (app z (int 1))

w2 :: R h ((Int -> a) -> a)
w2 = lam w1