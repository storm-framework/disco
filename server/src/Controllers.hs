{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers where

-- import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , runReaderT
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Database.Persist.Sqlite        ( SqlBackend )
import qualified Control.Concurrent.MVar       as MVar
import qualified Text.Mustache.Types           as Mustache
import           Frankie.Auth
import           Frankie.Config

import           Binah.Actions
import           Binah.Frankie
import           Binah.Core
import           Binah.Infrastructure
-- import           Binah.Filters
import           Binah.Templates
import           Binah.Concurrent
import           Binah.JSON
import           Network.Socket                 ( PortNumber )

import           Model

data Config = Config
  { configAuthMethod :: !(AuthMethod (Entity User) Controller)
  , configTemplateCache :: !(MVar.MVar Mustache.TemplateCache)
  , configSMTP :: SMTPConfig
  , configSecretKey :: ByteString
  }

data SMTPConfig = SMTPConfig
  { smtpHost :: String
  , smtpPort :: PortNumber
  , smtpUser :: String
  , smtpPass :: String
  }

type Controller = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))

instance Frankie.Auth.HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

instance HasTemplateCache Config where
  getTemplateCache = configTemplateCache

type Task = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config TIO))

runTask :: Task () -> Controller ()
runTask task = do
  backend <- lift ask
  cfg     <- getConfigT
  flip mapTaggedT task $ \t -> do
    forkTIO $ configure cfg (t `runReaderT` backend)
  return ()

{-@ checkOrganizer :: u: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ {v: () | IsOrganizer u}
  @-}
checkOrganizer :: Entity User -> Controller ()
checkOrganizer user = do
  level <- project userLevel' user
  if level == "organizer" then return () else respondError status403 Nothing
