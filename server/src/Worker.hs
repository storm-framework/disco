{-# LANGUAGE FlexibleContexts #-}
module Worker where


import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend )

import qualified Control.Concurrent            as C

import           Binah.Infrastructure

type Worker = TaggedT (ReaderT SqlBackend TIO)

-- TODO: Figure out the types
runWorker :: (MonadTIO m, MonadReader SqlBackend m) => Worker () -> TaggedT m ()
runWorker worker = do
  backend <- ask
  liftTIO $ TIO $ C.forkIO $ runTIO $ runReaderT (unTag worker) backend
  return ()
