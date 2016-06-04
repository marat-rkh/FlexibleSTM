module LockedVar (
    LockedVar,
    newLockedVar,
    readLockedVar,
    writeLockedVar
) where

import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Control.Monad (void)

newtype LockedVar a = LockedVar (MVar a)

newLockedVar :: a -> IO (LockedVar a)
newLockedVar x = LockedVar <$> newMVar x

readLockedVar :: LockedVar a -> IO a
readLockedVar (LockedVar mvar) = readMVar mvar

writeLockedVar :: LockedVar a -> a -> IO ()
writeLockedVar (LockedVar mvar) x = void $ swapMVar mvar x
