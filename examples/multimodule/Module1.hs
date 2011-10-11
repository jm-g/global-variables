module Module1 (
    increaseBy2
)where

import Data.Global
import Data.IORef

someVar :: IORef Int
someVar = declareIORef "my-global-var" 0

increaseBy2 :: IO ()
increaseBy2 = atomicModifyIORef someVar inc2
  where
    inc2 x = (2+x, ())
