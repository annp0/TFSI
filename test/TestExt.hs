module TestExt where

import Core
import Ext

test8 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, (Int -> Int, (Int, h0))) Int
test8 = (mul (s (s z)) (a (s z) (add z (int (-1)))))

test9 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, (Int -> Int, (Int, h0))) Int
test9 = ifel (lseq z (int 0)) (int 1) test8

test10 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, h0) ((Int -> Int) -> Int -> Int) 
test10 = l (l test9)

test11 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, h0) (Int -> Int)
test11 = fix test10

test12 :: (Core exp, ExtBool exp, ExtFix exp) => exp () (Int -> Int -> Int)
test12 = l test11

test13 :: (Core exp, ExtBool exp, ExtFix exp) => exp () (Int)
test13 = a (a test12 (int 7)) (int 2)

test14 :: (Core exp, ExtBool exp, ExtFix exp) => exp () (Int)
test14 = (a (l (add z (int 2))) (int 1))