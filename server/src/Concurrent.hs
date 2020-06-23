{-# LANGUAGE FlexibleContexts #-}
module Concurrent where


import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend )

import qualified Control.Concurrent            as C

import           Binah.Core
import           Binah.Infrastructure
import           Model

-- TODO: Figure out the types
forkTIO :: TaggedT TIO () -> TaggedT TIO C.ThreadId
forkTIO = TaggedT . TIO . C.forkIO . runTIO . unTag
