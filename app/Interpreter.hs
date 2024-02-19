{-# LANGUAGE InstanceSigs #-}
module Interpreter where
import Control.Monad (liftM)

class AbsExp exp where
    lit :: Int -> exp
    neg :: exp -> exp
    add :: exp -> exp -> exp

instance AbsExp Int where
    lit n = n
    neg n = - n
    add :: Int -> Int -> Int
    add m n = m + n

instance AbsExp String where
    lit = show
    neg n = "(-" ++ n ++ ")"
    add m n = "("++ m ++ "+" ++ n ++ ")"

data Tree = Leaf String | Node String [Tree]
            deriving (Eq, Read, Show)

instance AbsExp Tree where
    lit n = Node "lit" [Leaf $ show n]
    neg n = Node "neg" [n]
    add m n = Node "add" [m, n]



eval :: Int -> Int
eval = id

view :: String -> String
view = id

toTree :: Tree -> Tree
toTree = id

tf :: (AbsExp exp) => exp
tf = add (lit 8) (neg (add (lit 1) (lit 2)))

instance (AbsExp exp, AbsExp exp') => AbsExp (exp, exp') where
    lit x = (lit x, lit x)
    neg (x1, x2) = (neg x1, neg x2)
    add (x11, x12) (x21, x22) = ((add x11 x21), (add x12 x22))

duplicate :: (AbsExp exp, AbsExp exp') => (exp, exp') -> (exp, exp')
duplicate = id

type Err = String

safeRead :: (Read a) => String -> Either Err a
safeRead s = case reads s of
    [(x, "")] -> Right x
    _ -> Left $ "Read error: " ++ s

fromTree :: (AbsExp exp) => Tree -> Either Err exp
fromTree (Node "lit" [Leaf n]) = lit <$> safeRead n
fromTree (Node "neg" [e]) = neg <$> fromTree e
fromTree (Node "add" [e1, e2]) = add <$> (fromTree e1) <*> (fromTree e2)
fromTree e = Left $ "Invalid Tree: " ++ show e

checkConsume :: (AbsExp exp) => (exp -> IO ()) -> Either Err exp -> IO ()
checkConsume f (Left e) = putStrLn $ "Err: " ++ e
checkConsume f (Right x) = f x

dupConsume :: (Show a, AbsExp t, AbsExp b) => (t -> a) -> (t, b) -> IO b
dupConsume ev x = do
    let (x1, x2) = x
    print (ev x1)
    return x2

thrice x = do
    x1 <- dupConsume eval x
    x2 <- dupConsume view x1
    print $ toTree x2

tftree = toTree tf

sth = checkConsume thrice . fromTree $ tftree

fromTreeExt :: (AbsExp exp) => (Tree -> Either Err exp) -> (Tree -> Either Err exp)
fromTreeExt self (Node "lit" [Leaf n]) = lit <$> safeRead n
fromTreeExt self (Node "neg" [e]) = neg <$> self e
fromTreeExt self (Node "add" [e1, e2]) = add <$> self e1 <*> self e2
fromTreeExt self e = Left $ "Invalid tree: " ++ show e

fix :: (a -> a) -> a
fix f = f $ fix f

fromTree' :: (AbsExp exp) => (Tree -> Either Err exp)
fromTree' = fix fromTreeExt

