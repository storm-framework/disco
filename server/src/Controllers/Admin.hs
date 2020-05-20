{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Admin where

import           Data.Text                      ( Text )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Generics

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie

import           Controllers
import           Model
import           JSON

data UserData = UserData
  { userFullName     :: Text
  , userEmailAddress :: Text
  }
  deriving Generic

instance FromJSON UserData where
  parseJSON = genericParseJSON defaultOptions

newtype SendInvitationsReq = SendInvitationsReq
  { reqUsers :: [UserData]
  }
  deriving Generic

instance FromJSON SendInvitationsReq where
  parseJSON = genericParseJSON defaultOptions

{-@ sendInvitations :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
sendInvitations :: Controller ()
sendInvitations = do
  viewer                     <- requireAuthUser
  _                          <- requireOrganizer viewer
  (SendInvitationsReq users) <- decodeBody
  let invitations = map (\(UserData f e) -> mkInvitation "code" f e False) users
  ids <- insertMany invitations
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])
