module Data.Global.TVar.Test where

import Control.Concurrent ( myThreadId )
import Control.Concurrent.STM ( atomically )
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Global.TVar as G

(<==>) ::  Bool -> Bool -> Bool
x <==> y = (x && y) || (not x && not y)

prop_pure_declare :: String -> Bool
prop_pure_declare n = declare n == declare n

prop_bijective_declare :: String -> String -> Bool
prop_bijective_declare n1 n2 = (n1 == n2) <==> (declare n1 == declare n2)

prop_writeread :: String -> Integer -> Property
prop_writeread n z = monadicIO $
 do { let x = declare n
    ; run $ atomically $ x %= z
    ; Just z' <- run $ atomically $ G.read x
    ; assert $ z' == z
    }

prop_writereadIO :: String -> Integer -> Property
prop_writereadIO n z = monadicIO $
 do { let x = declare n
    ; run $ atomically $ x %= z
    ; Just z' <- undefined -- run $ G.readIO x
    ; assert $ z' == z
    }


prop_wwr :: String -> String -> Integer -> Integer -> Property
prop_wwr n1' n2' z1 z2 = z1 /= z2 ==> monadicIO $
 do { tid <- run myThreadId
    ; let n1 = show tid ++ n1'
    ; let n2 = show tid ++ n2'    
    ; let k1 = declare n1
    ; let k2 = declare n2
    ; run $ atomically $ k1 %= z1
    ; run $ atomically $ k2 %= z2
    ; Just z1' <- run $ atomically $ G.read k1
    ; assert $ (n1 /= n2) <==> (z1' == z1)
    }

-- prop_wwrIO :: String -> String -> Integer -> Integer -> Property
-- prop_wwrIO n1' n2' z1 z2 = z1 /= z2 ==> monadicIO $
--  do { tid <- run $ myThreadId
--     ; let n1 = show tid ++ n1'
--     ; let n2 = show tid ++ n2'    
--     ; let k1 = declare n1 
--     ; let k2 = declare n2
--     ; run $ atomically $ k1 %= z1
--     ; run $ atomically $ k2 %= z2
--     ; Just z1' <- run $ G.readIO k1
--     ; assert $ (n1 /= n2) <==> (z1' == z1)
--     }

tests ::  Test
tests = testGroup "Data.Global.TVar"
    [ testProperty "declare is pure" prop_pure_declare
    , testProperty "declare is a bijective function" prop_bijective_declare
    , testProperty "basic write/read test" prop_writeread
    -- , testProperty "basic write/readIO test" prop_writereadIO
    , testProperty "write/read with interference test" prop_wwr
    ]

