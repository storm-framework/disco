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
respondJSON status a = respondTagged (jsonResponse status a)

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse status a = Response status [(hContentType, "application/json")] (encode a)

{-@ respondError :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
respondError :: Status -> Maybe String -> Controller a
respondError status error = respondTagged (errorResponse status error)

errorResponse :: Status -> Maybe String -> Response
errorResponse status error = Response status
                                      [(hContentType, "application/json")]
                                      (encodeError error)
 where
  encodeError Nothing  = encode $ object []
  encodeError (Just e) = encode $ object ["error" .= e]

notFoundJSON :: Response
notFoundJSON = errorResponse status404 Nothing

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
