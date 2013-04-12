module Data.Conduit.Mutable
  ( mutableSink
  ) where

import Data.Conduit
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.IO.Class

mutableSink :: (MonadIO m) => TVar (Sink a m ()) -> Sink a m ()
mutableSink s = 
  let go = do
      mx <- await
      case mx of 
        Nothing -> return ()
        Just x -> do
          s' <- liftIO $ readTVarIO s
          lift $ yield x $$ s'
          go
  in go
