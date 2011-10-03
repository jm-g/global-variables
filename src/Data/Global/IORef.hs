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

module Data.Global.IORef (

  -- * Type
    GVar ()

  -- * Declaring Variables
  , declare
  , declareT

  -- * Basic Interface
  , read
  , write
  , (%=)
  , Data.Global.IORef.modify
  , atomicModify

  -- * Cost Model
  -- $costModel

  -- * Reflection of GVars
  , allGVars


) where

import Control.Applicative ( (<$>) )
import Control.Arrow       ( first )
import Data.Dynamic        ( Typeable, Dynamic, fromDynamic, toDyn )
import Data.IORef          ( newIORef, IORef, readIORef, writeIORef
                           , modifyIORef, atomicModifyIORef )
import System.IO.Unsafe    ( unsafePerformIO )

import Prelude      hiding (read)

-- import Data.Global.IORef.Internal
import Data.Global.Registry


-- Things to be aware of:
-- * atomically is not allowed inside unsafePerformIO, because it could
--   lead to nested transactions.
-- * newIO of tskiplist is also not usable from unsafePerformIO.
-- * newTVarIO *is* usable from unsafePerformIO.
--
-- Conclusions
-- * The registry cannot be implemented with STM.
-- * The heap needs an aditional indirection in order to initialize it
--   lazily outside of unsafePerformIO. Since this root TVar is not
--   updated later on, it should not lead to contention.
--
-- Design Decisions
-- * In order to support testing, all functions are implemented with
--   explicit parameters for all state. Wrappers are used to remove this
--   parameters in the exported functions.
--
-- Open Quesitions:
-- * The TVar wrapping maybe a heap might be replaced by a TMVar, since
--   TMVar support being empty. But it is unclear to me whether this
--   would introduce more contention, see [1].
--
-- [1]: http://hackage.haskell.org/packages/archive/stm/2.2.0.1/doc/html/Control-Concurrent-STM-TMVar.html#v:readTMVar


-- ---------------------
-- -- GLOBAL REGISTRY --
-- ---------------------


{-# NOINLINE globalRegistry #-}
globalRegistry :: Registry Cell
globalRegistry = unsafePerformIO setupRegistryIO
-- INV: This MVar must never be an empty MVar!
-- Although it may point to an empty map.


newtype Cell = Cell (IORef Dynamic)
    deriving (Eq, Typeable)




-- -----------------------
-- -- EXPOSED FUNCTIONS --
-- -----------------------

lookupGVar :: String -> Cell
lookupGVar = unsafePerformIO . lookupIO Cell newIORef globalRegistry


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
-- declare = GVar . lookupGVar
declare n
    | c1 == c2  = c1
    | otherwise = declare n
  where
    c1 = declare' n
    c2 = declare'' n


declare', declare'' :: String -> GVar a
{-# NOINLINE declare' #-}
declare' = GVar . lookupGVar
{-# NOINLINE declare'' #-}
declare'' = GVar . lookupGVar


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
    -> IO (Maybe a) -- ^ the value stored
read (GVar (Cell k)) = fromDynamic <$> readIORef k


-- Fehlerbehandlung oder implizites Anlegen?

-- | 'write' set a GVar to a new value. The value is not forced.
write
    :: Typeable a 
    => GVar a -- ^ the variable to be set 
    -> a -- ^ the value to be set
    -> IO ()
write (GVar (Cell k)) val = writeIORef k (toDyn val)



-- | '%=' set a GVar to a new value. The value is not forced. This an
-- alias of 'write'.
(%=)
    :: Typeable a 
    => GVar a -- ^ the variable to be set 
    -> a -- ^ the value to be set
    -> IO ()
(%=) = write

modify :: Typeable a => (GVar a) -> (Maybe a -> a) -> IO ()
modify (GVar (Cell k)) f = modifyIORef k (toDyn . f . fromDynamic)

atomicModify :: Typeable a => (GVar a) -> (Maybe a -> (a, b)) -> IO b
atomicModify (GVar (Cell k)) f = atomicModifyIORef k 
                                    (first toDyn . f . fromDynamic)

-- sollte man auch das Iterieren über alle GVars erlauben?

-- Design decisions
--
-- There are (at least) two possible semantics of allGVars 
--
-- 1. all variables declared 
--
-- 2. all variables declareed and currently set
--
-- 1. Has the advantage of being cheaper to implement, but is subject to the
-- same problems of hGetContents, aka. lazy IO. The result of an IO action
-- (allGVars) depends on the evaluation order of pure expressions containing
-- calls to declare. Implementing 1. would also mean to expose garbage
-- collection of declared variables. Therefore I'd say we implement variant 2.

-- | List all declared and existing GVars.
allGVars :: IO [String] 
allGVars = undefined



-- $costModel
-- This module is designed to follow a simple cost model.
--
-- 1. The declaration of a 'GVar' is quite expensive, assume @O(log n)@
-- where @n@ is the amount of GVars declared so far. All declarations
-- are synchronized, i.e. sequentialized.
--
-- 2. Reading and writing of a 'GVar' is a constant cost operation
-- which may block on concurrent reads or writes on the same
-- 'GVar'. Reading and writing to different GVars does not lead block.



