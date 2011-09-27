{-# LANGUAGE DeriveDataTypeable #-}
module Data.Global.Internal where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Map as M
import Data.IORef
import System.IO.Unsafe


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


type Registry = MVar (Map String GVar)


-- ------------------
-- -- GLOBAL STATE --
-- ------------------


{-# NOINLINE globalRegistry #-}
globalRegistry :: Registry
globalRegistry = unsafePerformIO $ newMVar M.empty
-- INV: This MVar must never be an empty MVar!
-- Although it may point to an empty map.


newtype GVar = GVar (IORef Dynamic)
  deriving (Eq, Typeable)

-- -----------------------
-- -- EXPOSED FUNCTIONS --
-- -----------------------


lookupGVar :: String -> GVar
lookupGVar = unsafePerformIO . lookupGVarIO globalRegistry


allGVars :: IO [String]
allGVars = undefined



-- -------------------------
-- -- TESTABLE PRIMITIVES --
-- -------------------------


lookupGVarIO :: Registry -> String -> IO GVar
lookupGVarIO registry name = modifyMVar registry lkup
  where
    lkup reg = case M.lookup name reg of
        Just k' -> return (reg, k')
        Nothing -> do
            k' <- GVar <$> (newIORef $ toDyn ())
            return (M.insert name k' reg, k')



read :: GVar -> IO Dynamic
read (GVar ref) = readIORef ref

write :: GVar -> Dynamic -> IO ()
write (GVar ref) val = writeIORef ref val

