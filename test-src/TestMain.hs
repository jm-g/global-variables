module Main where

import Test.Framework            (defaultMain)
import Data.Global.Registry.Test



main ::  IO ()
main = defaultMain [ tests ]
