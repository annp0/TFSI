module Test where

import Core
import Ext

test1 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, (Int -> Int, (Int, h0))) Int
test1 = (mul (s (s z)) (a (s z) (add z (int (-1)))))

test2 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, (Int -> Int, (Int, h0))) Int
test2 = ifel (lseq z (int 0)) (int 1) test1

test3 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, h0) ((Int -> Int) -> Int -> Int) 
test3 = l (l test2)

test4 :: (Core exp, ExtBool exp, ExtFix exp) => exp (Int, h0) (Int -> Int)
test4 = fix test3

test5 :: (Core exp, ExtBool exp, ExtFix exp) => exp () (Int -> Int -> Int)
test5 = l test4

test6 :: (Core exp, ExtBool exp, ExtFix exp) => exp () (Int)
test6 = a (a test5 (int 7)) (int 2)