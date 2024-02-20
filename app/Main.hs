module Main where

import Core
import Ext
import Test

main :: IO ()
main = do
    putStrLn $ show (eval test6)
    putStrLn $ view test6