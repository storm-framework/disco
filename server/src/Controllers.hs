{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers where

import           Data.Aeson
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend )
import           Frankie.Auth
import           Frankie.Config

import           Binah.Actions
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

instance HasSqlBackend Config where
  getSqlBackend = configBackend

{-@ respondJSON :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
respondJSON :: ToJSON a => Status -> a -> Controller b
respondJSON status a = do
  let body = encode a
  respondTagged $ Response status [(hContentType, "application/json")] body

{-@ respondError :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
respondError :: Status -> Maybe String -> Controller a
respondError status error = respondJSON status (object ["error" .= error])


-- TODO refine liftTIO
{-@ ignore decodeBody @-}
{-@ decodeBody :: TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
decodeBody :: FromJSON a => Controller a
decodeBody = do
  req  <- request
  body <- liftTIO $ reqBody req
  case decode body of
    Nothing -> respondError status400 Nothing
    Just a  -> return a

{-@ requireOrganizer ::
  u: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ {v: () | IsOrganizer u}
@-}
requireOrganizer :: Entity User -> Controller ()
requireOrganizer user = do
  level <- project userLevel' user
  if level == "organizer" then return () else respondError status403 Nothing
