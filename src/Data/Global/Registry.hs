module Data.Global.Registry where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Map as M



type Registry cell = MVar (Map String cell)


setupRegistryIO :: IO (Registry cell)
setupRegistryIO = newMVar $ M.empty



lookupIO :: (ref Dynamic -> cell)            -- GVar 
         -> (Dynamic -> IO (ref Dynamic))    -- newIORef
         -> (Registry cell) 
         -> String 
         -> IO cell
lookupIO wrapper maker registry name = modifyMVar registry lkup
  where
    lkup reg = case M.lookup name reg of
        Just k' -> return (reg, k')
        Nothing -> do
            k' <- wrapper <$> (maker $ toDyn ())
            return (M.insert name k' reg, k')


