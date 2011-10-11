module Module2 (
    tripple
) where


import Data.Global
import Data.IORef

someVar :: IORef Int
someVar = declareIORef "my-global-var" 0

tripple :: IO ()
tripple = atomicModifyIORef someVar tripple'
  where
    tripple' x = (x*3, ())
