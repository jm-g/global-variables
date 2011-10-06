{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Data.Global.Registry (
    declareIORef, declareMVar, declareTVar

  , lookupOrInsert
  , setupRegistry
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.IORef
import Data.Dynamic
import Data.Map as M
import GHC.Conc (pseq)

import System.IO.Unsafe


#if __GLASGOW_HASKELL__ >= 702
type Registry = Map (TypeRep,String) Dynamic
#else
type Registry = Map (Int,String) Dynamic
#endif

setupRegistry :: IO (MVar Registry)
setupRegistry = m `pseq` newMVar m
  where
	m = M.empty



{-# NOINLINE globalRegistry #-}
globalRegistry :: MVar Registry
globalRegistry = m `pseq` unsafePerformIO (newMVar m)
  where
    m = M.empty


-- TODO: add a proper assertion explaining the problem


lookupOrInsert
    :: forall a. forall ref. (Typeable a, Typeable1 ref)
    => MVar Registry
    -> (a -> IO (ref a))
    -> String
    -> a
    -> IO (ref a)
lookupOrInsert registry new name val
    | registry `pseq` new `pseq` name `pseq` val `pseq` False = undefined
lookupOrInsert registry new name val = modifyMVar registry lkup
  where
    typ = typeOf val
    err exp got = error $ "Data.Global.Registry: Invariant violation\n"
                       ++ "expected: " ++ show exp ++ "\n"
                       ++ "got: " ++ show got ++ "\n" 

#if __GLASGOW_HASKELL__ >= 702
    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = case M.lookup (typ, name) reg of
        Just ref -> return (reg, fromDyn ref (err typ (dynTypeRep ref)))
        Nothing -> 
         do { ref <- new val
            ; return (M.insert (typ, name) (toDyn ref) reg, ref)
            }
#else
    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = 
     do { typIdx <- typeRepKey typ 
        ; case M.lookup (typIdx, name) reg of
            Just ref -> return (reg, fromDyn ref (err typ (dynTypeRep ref)))
            Nothing -> 
             do { ref <- new val
                ; return (M.insert (typIdx, name) (toDyn ref) reg, ref)
                }
        }
#endif

{-# NOINLINE lookupOrInsert #-}


lookupOrInsertIORef
    :: Typeable a
    => String
    -> a
    -> IO (IORef a)
lookupOrInsertIORef = lookupOrInsert globalRegistry newIORef
{-# NOINLINE lookupOrInsertIORef #-}


lookupOrInsertMVar
    :: Typeable a
    => String
    -> a
    -> IO (MVar a)
lookupOrInsertMVar = lookupOrInsert globalRegistry newMVar



lookupOrInsertTVar
    :: Typeable a
    => String
    -> a
    -> IO (TVar a)
lookupOrInsertTVar = lookupOrInsert globalRegistry newTVarIO



declareIORef, declareIORef', declareIORef''
    :: Typeable a
    => String
    -> a
    -> (IORef a)
declareIORef name val
    | res1 == res2 = res1
    | otherwise    = declareIORef name val
  where
    res1 = declareIORef' name val
    res2 = declareIORef'' name val

declareIORef' name val = unsafePerformIO $ lookupOrInsertIORef name val
{-# NOINLINE declareIORef' #-}

declareIORef'' name val = unsafePerformIO $ lookupOrInsertIORef name val
{-# NOINLINE declareIORef'' #-}



declareMVar, declareMVar', declareMVar''
    :: Typeable a
    => String
    -> a
    -> (MVar a)
declareMVar name val
    | res1 == res2 = res1
    | otherwise    = declareMVar name val
  where
    res1 = declareMVar' name val
    res2 = declareMVar'' name val

declareMVar' name val = unsafePerformIO $ lookupOrInsertMVar name val
{-# NOINLINE declareMVar' #-}
declareMVar'' name val = unsafePerformIO $ lookupOrInsertMVar name val
{-# NOINLINE declareMVar'' #-}



declareTVar, declareTVar', declareTVar''
    :: Typeable a
    => String
    -> a
    -> (TVar a)
declareTVar name val
    | res1 == res2 = res1
    | otherwise    = declareTVar name val
  where
    res1 = declareTVar' name val
    res2 = declareTVar'' name val

declareTVar' name val = unsafePerformIO $ lookupOrInsertTVar name val
{-# NOINLINE declareTVar' #-}
declareTVar'' name val = unsafePerformIO $ lookupOrInsertTVar name val
{-# NOINLINE declareTVar'' #-}
