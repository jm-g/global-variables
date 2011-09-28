{-# LANGUAGE DeriveDataTypeable #-}
-- | 'Data.Global' provides a kind of global variables - 'GVar' - tailored to
-- solve the /configuration problem/. 'GVar's are global variables
-- identified by a name. This allows GVars to be obtained in pure
-- code.
--
-- The following two lines can be added to as many modules as you
-- want. And 'aGlobalVar' will always refer to the same global
-- variable.
-- 
-- > aGlobalVar :: GVar Integer
-- > aGlobalVar = declare "my-global-variable-name"
--
-- Note, there is no need to use 'unsafePerformIO' or @NOINLINE@
-- pragmas. Yet GVars can be used similar to IORefs.

module Data.Global.TVar (
  -- * Type
    GVar ()
  
  -- * Declaring Variables
  , declare
  , declareT
  
  -- * Basic Interface
  , read
  , write
  , (%=)
  , Data.Global.TVar.readIO
  -- * Cost Model
  -- $costModel
  
  -- * Reflection of GVars
  , allKeys
) where

import Control.Applicative    ( (<$>) )
import Control.Concurrent.STM ( STM, TVar, newTVarIO, readTVar, writeTVar
                              , readTVarIO )
import Data.Dynamic           ( Typeable, Dynamic, fromDynamic, toDyn )
import System.IO.Unsafe       ( unsafePerformIO )
                                   
import Prelude      hiding    (read)


import Data.Global.Registry


-- ---------------------
-- -- GLOBAL REGISTRY --
-- ---------------------


{-# NOINLINE globalRegistry #-}
globalRegistry :: Registry Cell
globalRegistry = unsafePerformIO setupRegistryIO
-- INV: This MVar must never be an empty MVar!
-- Although it may point to an empty map.


newtype Cell = Cell (TVar Dynamic)
  deriving (Eq, Typeable)

-- -----------------------
-- -- EXPOSED FUNCTIONS --
-- -----------------------


lookupGVar :: String -> Cell
lookupGVar = unsafePerformIO . lookupIO Cell newTVarIO globalRegistry


allKeys :: IO [String]
allKeys = undefined

-- ----------
-- -- GVar --
-- ----------

-- TODO:
-- - use System.Mem.Weak and finalizers to collect garbage.

-- Strictness requirements:
--
-- In order to avoid a nested atomically exception, due to unsafePerformIO
-- nesting STM contexts, it is necessary to ensure that any call to
-- Data.Global.Internal.lookupGVar is not executed inside an atomically block.
--
--
-- To achieve this, we follow the following two principles:
--
-- 1. The unique value is strictly stored in GVar constructor.
--
-- 2. Every function has to pattern match on a value of type GVar before it is
-- handed to a atomically block.

newtype GVar a = GVar Cell
  deriving (Eq, Typeable)



-- ----------------
-- -- PRIMITIVES --
-- ----------------

-- | 'declare' declares the existence of a 'GVar', effectively
-- creating it. Note that multiples calls to the same logical name
-- return the exact same 'GVar'.
declare
    :: String -- ^ logical name 
    -> GVar a -- ^ the global variable identified by the logical name
declare = GVar . lookupGVar


-- | 'declareT' declares the existence of a 'GVar', effectively
-- creating it. Note that multiples calls to the same logical name
-- return the exact same 'GVar'.
declareT 
    :: a -- ^ A witness to determine the type of the GVar to be
    -- created. This value is ignored.  
    -> String -- ^ logical name
    -> GVar a -- ^ the global variable identified by the logical name
declareT _ = declare



-- | 'read' reads the value of a GVar. 'Nothing' is returned if this
-- variable was never written, 'unset' was called, or the stored value
-- has the wrong type. 
read
    :: Typeable a 
    => GVar a -- ^ the variable to be read 
    -> STM (Maybe a) -- ^ the value stored
read (GVar (Cell k)) = fromDynamic <$> readTVar k

-- | 'read' reads the value of a GVar. 'Nothing' is returned if this
-- variable was never written, 'unset' was called, or the stored value
-- has the wrong type. 
readIO
    :: Typeable a 
    => GVar a -- ^ the variable to be read 
    -> IO (Maybe a) -- ^ the value stored
readIO (GVar (Cell k)) = fromDynamic <$> readTVarIO k


-- Fehlerbehandlung oder implizites Anlegen?

-- | 'write' set a GVar to a new value. The value is not forced.
write
    :: Typeable a 
    => GVar a -- ^ the variable to be set 
    -> a -- ^ the value to be set
    -> STM ()
write (GVar (Cell k)) val = writeTVar k (toDyn val)


-- | '%=' set a GVar to a new value. The value is not forced. This an
-- alias of 'write'.
(%=)
    :: Typeable a 
    => GVar a -- ^ the variable to be set 
    -> a -- ^ the value to be set
    -> STM ()
(%=) = write
