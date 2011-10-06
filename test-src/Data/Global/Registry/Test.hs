module Data.Global.Registry.Test where

import Control.Concurrent ( myThreadId, forkIO )
import qualified Control.Exception as E (assert)
import Control.Monad ( forM )
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

(<==>) ::  Bool -> Bool -> Bool
x <==> y = (x && y) || (not x && not y)

prop_idempotent_lookupIO :: String -> Property
prop_idempotent_lookupIO n = monadicIO $
 do { reg <- run setupRegistry
    ; c1 <- run $ lookupOrInsert reg newIORef n ()
    ; c2 <- run $ lookupOrInsert reg newIORef n ()
    ; assert $ c1 == c2
    }


prop_safe_lookupIO :: String -> String -> Property
prop_safe_lookupIO n1 n2 = monadicIO $
 do { reg <- run setupRegistry
    ; c1 <- run $ lookupOrInsert reg newIORef n1 ()
    ; c2 <- run $ lookupOrInsert reg newIORef n2 ()
    ; assert $ (n1 == n2) ===> (c1 == c2)
    }

prop_pure_declare :: String -> Bool
prop_pure_declare n = declareIORef n () == declareIORef n ()

prop_bijective_declare :: String -> String -> Bool
prop_bijective_declare n1 n2 = (n1 == n2) <==> (declareIORef n1 () == declareIORef n2 ())

prop_bijective_declare_forward :: String -> String -> Bool
prop_bijective_declare_forward n1 n2 = (n1 == n2) ===> (declareIORef n1 () == declareIORef n2 ())

prop_bijective_declare_backward :: String -> String -> Bool
prop_bijective_declare_backward n1 n2 = (declareIORef n1 () == declareIORef n2 ()) ===> (n1 == n2) 


prop_writeread :: String -> Integer -> Property
prop_writeread n z = monadicIO $
 do { let x = declareIORef n 1
    ; run $ x `writeIORef` z
    ; z' <- run $ readIORef x
    ; assert $ z' == z
    }


prop_wwr :: String -> String -> Integer -> Integer -> Property
prop_wwr n1' n2' z1 z2 = z1 /= z2 ==> monadicIO $
 do { tid <- run myThreadId
    ; let n1 = show tid ++ n1'
    ; let n2 = show tid ++ n2'
    ; let k1 = declareIORef n1 1
    ; let k2 = declareIORef n2 1
    ; run $ k1 `writeIORef` z1
    ; run $ k2 `writeIORef` z2
    ; z1' <- run $ readIORef k1
    ; assert $ (n1 /= n2) <==> (z1' == z1)
    }


test_conc :: String -> Property
test_conc _ = monadicIO $ run $
    forM [1 :: Int ..100] $ \_ -> forkIO $ do
        x <- return $ declareIORef "foo" (1 :: Int)
        y <- return $ declareIORef "foo" (1 :: Int)
        E.assert (x == y) $ return ()

tests ::  Test
tests = testGroup "Data.Global.Registry"
    [ testProperty "lookupIO is idempotent" prop_idempotent_lookupIO
    , testProperty "lookupIO is safe" prop_safe_lookupIO
    , testProperty "declareIORef is pure" prop_pure_declare

    -- These tests are redundant, but they proved to be valuable when debugging concurrency issues.
    , testProperty "declareIORef is a bijective function (forward)" prop_bijective_declare_forward
    , testProperty "declareIORef is a bijective function (backward)" prop_bijective_declare_backward
    , testProperty "declareIORef is a bijective function" prop_bijective_declare

    , testProperty "basic write/read test" prop_writeread
    , testProperty "write/read with interference test" prop_wwr
    , testProperty "conc" test_conc
    , testProperty "conc" test_conc
    ]
