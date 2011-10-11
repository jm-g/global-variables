-- -*- encoding: utf-8; fill-column: 95 -*-

{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------
-- |
-- Module        : Data.Global.Registry
-- Creation Date : 2011-09-01
-- Authors       : Jean-Marie Gaillourdet <jmg@gaillourdet.net>
-- License       : BSD-style
-- Portability   : all
--
-- The internal module.
-----------------------------------------------------------------------------------------------
module Data.Global.Registry (
  -- * Public Interface
    declareIORef, declareMVar, declareTVar

  -- * Private Testing Interface
  , lookupOrInsert
  , setupRegistry
) where

#if __GLASGOW_HASKELL__ < 702
import Control.Exception       ( evaluate )
#endif
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.IORef
import Data.Dynamic
import Data.Map as M
import GHC.Conc (pseq)

import System.IO.Unsafe


#if __GLASGOW_HASKELL__ >= 702
type Registry = Map (TypeRep,TypeRep,String) Dynamic
#else
type Registry = Map (Int,Int,String) Dynamic
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
    typVal = typeOf val
    typRef = typeOf (undefined :: ref ()) -- TypeRep representing the reference, e.g. IORef,
                                          -- MVar

    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = case M.lookup (typRef, typVal, name) reg of
        Just ref -> return (reg, fromDyn ref (err typVal (dynTypeRep ref)))
        Nothing ->
         do { ref <- new val
            ; return (M.insert (typRef, typVal, name) (toDyn ref) reg, ref)
            }
#else
    lkup :: Registry -> IO (Registry, ref a)
    lkup reg =
     do { typVal <- typeOf' val
        ; typRef <- typeOf' (undefined :: ref ()) -- TypeRep representing the reference,
                                                  -- e.g. IORef, MVar
        ; typValIdx <- typeRepKey typVal
        ; typRefIdx <- typeRepKey typRef
        ; case M.lookup (typRefIdx, typValIdx, name) reg of
            Just ref -> return (reg, fromDyn ref (err typVal (dynTypeRep ref)))
            Nothing ->
             do { ref <- new val
                ; _ <- typeOf' ref
                ; return (M.insert (typRefIdx, typValIdx, name) (toDyn ref) reg, ref)
                }
        }

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

-- Ugly workaround to http://hackage.haskell.org/trac/ghc/ticket/5540
typeOf' :: Typeable a => a -> IO TypeRep
typeOf' x =
 do { () <- takeMVar lock
    ; t <- evaluate $ typeOf x
    ; putMVar lock ()
    ; return t
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
{-# NOINLINE lookupOrInsertMVar #-}



lookupOrInsertTVar
    :: Typeable a
    => String
    -> a
    -> IO (TVar a)
lookupOrInsertTVar = lookupOrInsert globalRegistry newTVarIO
{-# NOINLINE lookupOrInsertTVar #-}


-- | @declareIORef name val@ maps a variable name to an 'IORef'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'IORef'.
--
-- @
-- someVar :: IORef Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'IORef's.
declareIORef
    :: Typeable a
    => String     -- ^ The identifying name
    -> a          -- ^ The initial value of the 'IORef', it may or may not be used.
    -> (IORef a)  -- ^ A unique 'IORef' determined by @(name, typeOf val)@. Whether it refers
                  -- to the given initial value or not is unspecified.
declareIORef name val = unsafePerformIO $ lookupOrInsertIORef name val
{-# NOINLINE declareIORef #-}



-- | @declareMVar name val@ maps a variable name to an 'MVar'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'MVar'.
--
-- @
-- someVar :: MVar Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'MVar's.
declareMVar
    :: Typeable a
    => String    -- ^ The identifying name
    -> a         -- ^ The initial value of the 'MVar', it may or may not be used.
    -> (MVar a)  -- ^ A unique 'MVar' determined by @(name, typeOf val)@. Whether it refers to
                 -- the given initial value or not is unspecified.
declareMVar name val = unsafePerformIO $ lookupOrInsertMVar name val
{-# NOINLINE declareMVar #-}



-- | @declareTVar name val@ maps a variable name to an 'TVar'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'TVar'.
--
-- @
-- someVar :: TVar Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'TVar's.
declareTVar
    :: Typeable a
    => String    -- ^ The identifying name
    -> a         -- ^ The initial value of the 'TVar', it may or may not be used.
    -> (TVar a)  -- ^ A unique 'TVar' determined by @(name, typeOf val)@. Whether it refers to
                 -- the given initial value or not is unspecified.
declareTVar name val = unsafePerformIO $ lookupOrInsertTVar name val
{-# NOINLINE declareTVar #-}
