{-# LANGUAGE RankNTypes #-}
module Exception
  ( module Control.Exception
  , tryT
  )
where

import           Control.Exception
import           Binah.Infrastructure
import           Binah.Core
import           Model


tryT
  :: (MonadTIO n, Exception e) => (forall m . MonadTIO m => TaggedT m a) -> TaggedT n (Either e a)
tryT act = TaggedT $ liftTIO $ TIO $ try $ runTIO $ unTag act
