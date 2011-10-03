module Data.Global.Registry where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Map as M
import GHC.Conc (pseq)



type Registry cell = MVar (Map String cell)


setupRegistryIO :: IO (Registry cell)
setupRegistryIO = m `pseq` newMVar m
  where
	m = M.empty




lookupIO :: (ref Dynamic -> cell)            -- GVar 
         -> (Dynamic -> IO (ref Dynamic))    -- newIORef
         -> Registry cell 
         -> String 
         -> IO cell
lookupIO wrapper maker registry name = modifyMVar registry lkup
 -- do { old <- takeMVar registry
 --    ; (new, res) <- lkup old
 --    -- ; evaluate res
 --    -- ; evaluate new
 --    ; putMVar registry new
 --    ; return res
 --    }
  where
    lkup reg = case M.lookup name reg of
        Just k' -> return (reg, k')
        Nothing -> do
            k' <- wrapper <$> maker (toDyn ())
            return (M.insert name k' reg, k')


