{-# LANGUAGE DeriveDataTypeable #-}
module Data.Global.IORef.Internal where

import Control.Concurrent.MVar
import GHC.Conc
import Data.Dynamic        ( Typeable, Dynamic, fromDynamic, toDyn )
import Data.IORef          ( newIORef, IORef, readIORef, writeIORef
                           , modifyIORef, atomicModifyIORef )
import Data.Map as M
import System.IO.Unsafe    ( unsafePerformIO )

import Data.Global.Registry


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

