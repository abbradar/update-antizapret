module Control.Concurrent.STM.TEVar
  ( TEVar
  , newTEVar
  , newTEVarIO
  , writeTEVar
  , readTEVar
  , waitTEVar
  ) where

import Control.Monad
import Control.Concurrent.STM

-- STM variable with a flag to indicate whether it has updated since the last read.
newtype TEVar a = TEVar (TVar (Bool, a))

newTEVar :: Bool -> a -> STM (TEVar a)
newTEVar initialFlag value = TEVar <$> newTVar (initialFlag, value)

newTEVarIO :: Bool -> a -> IO (TEVar a)
newTEVarIO initialFlag value = TEVar <$> newTVarIO (initialFlag, value)

writeTEVar :: TEVar a -> a -> STM ()
writeTEVar (TEVar var) = writeTVar var . (True, )

readTEVar :: TEVar a -> STM (Bool, a)
readTEVar (TEVar var) = do
  ret@(changed, val) <- readTVar var
  when changed $ writeTVar var (False, val)
  return ret

waitTEVar :: TEVar a -> STM a
waitTEVar (TEVar var) = do
  (changed, val) <- readTVar var
  if changed
    then do
      writeTVar var (False, val)
      return val
    else retry
