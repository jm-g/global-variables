module Main where

import Test.Framework (defaultMain)

import Data.Global.Test as G
import Data.Global.Internal.Test as I


main ::  IO ()
main = defaultMain
    [ I.tests
    , G.tests
    ]
