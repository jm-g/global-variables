module Data.Global.Registry.Test ( tests ) where

import Control.Concurrent                   ( MVar )
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Global.Registry



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



prop_pure_declare :: Eq a => (String -> () -> a) -> String -> Bool
prop_pure_declare decl n = decl n () == decl n ()



prop_bijective_declare :: Eq a => (String -> () -> a) -> String -> String -> Bool
prop_bijective_declare decl n1 n2 = (n1 == n2) <==> (decl n1 () == decl n2 ())



prop_bijective_declare_forward :: Eq a => (String -> () -> a) -> String -> String -> Bool
prop_bijective_declare_forward decl n1 n2 = (n1 == n2) ===> (decl n1 () == decl n2 ())



prop_bijective_declare_backward :: Eq a => (String -> () -> a) -> String -> String -> Bool
prop_bijective_declare_backward decl n1 n2 = (decl n1 () == decl n2 ()) ===> (n1 == n2)



tests ::  Test
tests = testGroup "Data.Global.Registry"
    [ testProperty "lookupIO is idempotent" prop_idempotent_lookupIO
    , testProperty "lookupIO is safe" prop_safe_lookupIO

    , testGroup "IORef"
        [ testProperty "declareIORef is pure" $ prop_pure_declare declareIORef

        -- These tests are redundant, but they proved to be valuable when debugging concurrency issues.
        , testProperty "declareIORef is a bijective function (forward)" $ prop_bijective_declare_forward declareIORef
        , testProperty "declareIORef is a bijective function (backward)" $ prop_bijective_declare_backward declareIORef

        , testProperty "declareIORef is a bijective function" $ prop_bijective_declare declareIORef
        ]

    , testGroup "MVar"
        [ testProperty "declareMVar is pure" $ prop_pure_declare declareMVar

        -- These tests are redundant, but they proved to be valuable when debugging concurrency issues.
        , testProperty "declareMVar is a bijective function (forward)" $ prop_bijective_declare_forward declareMVar
        , testProperty "declareMVar is a bijective function (backward)" $ prop_bijective_declare_backward declareMVar

        , testProperty "declareMVar is a bijective function" $ prop_bijective_declare declareMVar
        ]

    , testGroup "empty MVar" $
        let decl :: String -> () -> MVar ()
            decl n _ = declareEmptyMVar n in
        [ testProperty "declareEmptyMVar is pure" $ prop_pure_declare decl

        -- These tests are redundant, but they proved to be valuable when debugging concurrency issues.
        , testProperty "declareEmptyMVar is a bijective function (forward)" $ prop_bijective_declare_forward decl
        , testProperty "declareEmptyMVar is a bijective function (backward)" $ prop_bijective_declare_backward decl

        , testProperty "declareEmptyMVar is a bijective function" $ prop_bijective_declare decl
        ]

    , testGroup "TVar"
        [ testProperty "declareTVar is pure" $ prop_pure_declare declareTVar

        -- These tests are redundant, but they proved to be valuable when debugging concurrency issues.
        , testProperty "declareTVar is a bijective function (forward)" $ prop_bijective_declare_forward declareTVar
        , testProperty "declareTVar is a bijective function (backward)" $ prop_bijective_declare_backward declareTVar

        , testProperty "declareTVar is a bijective function" $ prop_bijective_declare declareTVar
        ]
    ]
