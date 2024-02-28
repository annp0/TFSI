module Main where

import Core
import Ext
import TestCore
import TestExt
import Test.HUnit

testv = TestCase (assertEqual "eval-1" 3 (eval test7))
testv1 = TestCase (assertEqual "eval-2" 49 (eval test6))

main :: IO Counts
main = runTestTT $ TestList [TestLabel "testv" testv, TestLabel "testv1" testv1]