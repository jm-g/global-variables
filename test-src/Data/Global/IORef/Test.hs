module Data.Global.IORef.Test where

import Control.Concurrent ( myThreadId )
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Global.IORef as G

(<==>) ::  Bool -> Bool -> Bool
x <==> y = (x && y) || (not x && not y)

(===>) ::  Bool -> Bool -> Bool
x ===> y = (not x) || y


prop_pure_declare :: String -> Bool
prop_pure_declare n = declare n == declare n

prop_bijective_declare :: String -> String -> Bool
prop_bijective_declare n1 n2 = (n1 == n2) <==> (declare n1 == declare n2)

prop_bijective_declare_forward :: String -> String -> Bool
prop_bijective_declare_forward n1 n2 = (n1 == n2) ===> (declare n1 == declare n2)

prop_bijective_declare_backward :: String -> String -> Bool
prop_bijective_declare_backward n1 n2 = (declare n1 == declare n2) ===> (n1 == n2) 

prop_writeread :: String -> Integer -> Property
prop_writeread n z = monadicIO $
 do { let x = declare n
    ; run $ x %= z
    ; Just z' <- run $ G.read x
    ; assert $ z' == z
    }

prop_wwr :: String -> String -> Integer -> Integer -> Property
prop_wwr n1' n2' z1 z2 = z1 /= z2 ==> monadicIO $
 do { tid <- run $ myThreadId
    ; let n1 = show tid ++ n1'
    ; let n2 = show tid ++ n2'
    ; let k1 = declare n1
    ; let k2 = declare n2
    ; run $ k1 %= z1
    ; run $ k2 %= z2
    ; Just z1' <- run $ G.read k1
    ; assert $ (n1 /= n2) <==> (z1' == z1)
    }

tests ::  Test
tests = testGroup "Data.Global.IORef"
    [ testProperty "declare is pure" prop_pure_declare
    , testProperty "declare is a bijective function" prop_bijective_declare
    , testProperty "declare is a bijective function (forward)" prop_bijective_declare_forward
    , testProperty "declare is a bijective function (backward)" prop_bijective_declare_backward
    , testProperty "basic write/read test" prop_writeread
    , testProperty "write/read with interference test" prop_wwr
    ]

