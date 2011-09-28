module Main where

import Test.Framework (defaultMain)

import Data.Global.IORef.Test as G


main ::  IO ()
main = defaultMain
    [ G.tests
    ]
