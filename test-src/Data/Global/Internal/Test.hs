module Data.Global.Internal.Test where

import Control.Concurrent
import Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.Providers.QuickCheck2


import Data.Global.Internal



setupRegistry ::  IO (MVar (Map k a))
setupRegistry = newMVar M.empty

(<==>) ::  Bool -> Bool -> Bool
x <==> y = (x && y) || (not x && not y)


prop_referentialTransparencyLookupGVarInternal
    :: (IO () -> IO a)
    -> String
    -> String
    -> Property
prop_referentialTransparencyLookupGVarInternal fork name1 name2 = monadicIO $
  do { registry <- run setupRegistry
     ; res1 <- run newEmptyMVar
     ; res2 <- run newEmptyMVar
     ; _ <- run $ forkingLookup registry name1 res1
     ; _ <- run $ forkingLookup registry name2 res2
     ; k1 <- run $ takeMVar res1
     ; k2 <- run $ takeMVar res2
     ; assert $ (name1 == name2) <==> (k1 == k2)
     }
  where
    forkingLookup reg n res = fork $
      do { k <- lookupGVarIO reg n
         ; putMVar res k
         }

prop_referentialTransparencyLookupGVar
    :: (IO () -> IO a)
    -> String
    -> String
    -> Property
prop_referentialTransparencyLookupGVar fork name1 name2 =
    monadicIO $
      do { res1 <- run newEmptyMVar
         ; res2 <- run newEmptyMVar
         ; _ <- run $ fork (putMVar res1 (lookupGVar name1))
         ; _ <- run $ fork (putMVar res2 (lookupGVar name2))
         ; k1 <- run $ takeMVar res1
         ; k2 <- run $ takeMVar res2
         ; assert $ (name1 == name2) <==> (k1 == k2)
         }

tests :: Test
tests = testGroup "Data.Global.Internal.Test"
    [ testProperty "idempotence of lookupGVarInternal under forkIO" $
        prop_referentialTransparencyLookupGVarInternal forkIO
    , testProperty "idempotence of lookupGVarInternal under forkOS" $
        prop_referentialTransparencyLookupGVarInternal forkOS
    , testProperty "idempotence of lookupGVar under forkIO" $
        prop_referentialTransparencyLookupGVar forkIO
    , testProperty "idempotence of lookupGVar under forkOS" $
        prop_referentialTransparencyLookupGVar forkOS
    ]
