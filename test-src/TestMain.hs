module Main where

import Test.Framework (defaultMain)

import Data.Global.IORef.Test as I
import Data.Global.TVar.Test as T


main ::  IO ()
main = defaultMain
    [ I.tests
    , T.tests
    ]
