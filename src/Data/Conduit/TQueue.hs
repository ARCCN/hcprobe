{-# LANGUAGE RankNTypes, KindSignatures #-}
module Data.Conduit.TQueue
  ( sourceTQueue
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Internal

sourceTQueue :: (MonadIO m) => TQueue a -> Source m a
sourceTQueue q = ConduitM src
  where src = PipeM pull
        pull = do x <- liftSTM $ readTQueue q
                  return $ HaveOutput src close x
        close = return ()

liftSTM :: forall (m :: * -> *) a. MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
