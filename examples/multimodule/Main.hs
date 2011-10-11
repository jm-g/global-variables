module Main where

import Data.IORef
import Data.Global

import Module1
import Module2

main :: IO ()
main = do 
    increaseBy2
    tripple
    increaseBy2
    tripple
    let ref = declareIORef "my-global-var" 17 :: IORef Int
    print =<< readIORef ref
