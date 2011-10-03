module Data.Global.Registry.Test where

import Data.Dynamic
import Data.IORef   
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Global.Registry

newtype Cell = Cell (IORef Dynamic)
  deriving Eq

(===>) ::  Bool -> Bool -> Bool
x ===> y = not x || y


prop_idempotent_lookupIO :: String -> Property
prop_idempotent_lookupIO n = monadicIO $
 do { reg <- run setupRegistryIO
    ; c1 <- run $ lookupIO Cell newIORef reg n
    ; c2 <- run $ lookupIO Cell newIORef reg n
    ; assert $ c1 == c2
    }


prop_safe_lookupIO :: String -> String -> Property
prop_safe_lookupIO n1 n2 = monadicIO $
 do { reg <- run setupRegistryIO
    ; c1 <- run $ lookupIO Cell newIORef reg n1
    ; c2 <- run $ lookupIO Cell newIORef reg n2
    ; assert $ (n1 == n2) ===> (c1 == c2)
    }


tests ::  Test
tests = testGroup "Data.Global.Registry"
    [ testProperty "lookupIO is idempotent" prop_idempotent_lookupIO
    , testProperty "lookupIO is safe" prop_safe_lookupIO
    ]
