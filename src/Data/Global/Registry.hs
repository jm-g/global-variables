{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Data.Global.Registry (
    declareIORef, declareMVar, declareTVar

  , lookupOrInsert
  , setupRegistry
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
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

-- | Test helper
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

-- | Exposed for unit testing
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
    err ex got = error $ "Data.Global.Registry: Invariant violation\n"
                       ++ "expected: " ++ show ex ++ "\n"
                       ++ "got: " ++ show got ++ "\n" 

#if __GLASGOW_HASKELL__ >= 702
    typ = typeOf val

    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = case M.lookup (typ, name) reg of
        Just ref -> return (reg, fromDyn ref (err typ (dynTypeRep ref)))
        Nothing -> 
         do { ref <- new val
            ; return (M.insert (typ, name) (toDyn ref) reg, ref)
            }
#else
    typ = typeOf' val

    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = 
     do { typIdx <- typeRepKey typ 
        ; case M.lookup (typIdx, name) reg of
            Just ref -> return (reg, fromDyn ref (err typ (dynTypeRep ref)))
            Nothing -> 
             do { ref <- new val
                ; _ <- evaluate $ typeOf' ref
                ; return (M.insert (typIdx, name) (toDyn ref) reg, ref)
                }
        }


-- Ugly workaround to http://hackage.haskell.org/trac/ghc/ticket/5540
typeOf', typeOf'', typeOf''':: Typeable a => a -> TypeRep
typeOf' val
    | t1 == t2 = t1
    | otherwise = typeOf' val
  where
    t1 = typeOf'' val
    t2 = typeOf''' val
{-# NOINLINE typeOf' #-}


typeOf'' x = typeOf x
{-# NOINLINE typeOf'' #-}
typeOf''' x = typeOf x
{-# NOINLINE typeOf''' #-}

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
{-# NOINLINE lookupOrInsertMVar #-}



lookupOrInsertTVar
    :: Typeable a
    => String
    -> a
    -> IO (TVar a)
lookupOrInsertTVar = lookupOrInsert globalRegistry newTVarIO
{-# NOINLINE lookupOrInsertTVar #-}



declareIORef
    :: Typeable a
    => String
    -> a
    -> (IORef a)
declareIORef name val = unsafePerformIO $ lookupOrInsertIORef name val
{-# NOINLINE declareIORef #-}



declareMVar
    :: Typeable a
    => String
    -> a
    -> (MVar a)
declareMVar name val = unsafePerformIO $ lookupOrInsertMVar name val
{-# NOINLINE declareMVar #-}



declareTVar
    :: Typeable a
    => String
    -> a
    -> (TVar a)
declareTVar name val = unsafePerformIO $ lookupOrInsertTVar name val
{-# NOINLINE declareTVar #-}
