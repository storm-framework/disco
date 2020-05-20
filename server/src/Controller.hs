{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller where

import           Data.Aeson
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend )
import           Frankie.Auth
import           Frankie.Config

import           Binah.Frankie
import           Binah.Core
import           Binah.Infrastructure
import           Binah.Filters

import           Model

data Config = Config
  { configBackend :: SqlBackend
  , configAuthMethod :: !(AuthMethod (Entity User) Controller)
  }

type Controller = TaggedT (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))

instance Frankie.Auth.HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

{-@
respondJSON :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _
@-}
respondJSON :: ToJSON a => Status -> a -> Controller b
respondJSON status a = do
  let body = encode a
  respondTagged $ Response status [(hContentType, "application/json")] body

{-@
respond200 :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _
@-}
respond200 :: ToJSON a => a -> Controller b
respond200 = respondJSON status200

{-@
respond400 :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _
@-}
respond400 :: Maybe String -> Controller a
respond400 error = respondJSON status400 (object ["error" .= error])

{-@
respond401 :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _
@-}
respond401 :: Maybe String -> Controller a
respond401 error = respondJSON status401 (object ["error" .= error])
