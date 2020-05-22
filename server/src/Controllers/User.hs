{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import           Data.Text                      ( Text )
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
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

import           Controllers.Invitation         ( InvitationCode(..) )

import           Controllers
import           Model
import           JSON

-------------------------------------------------------------------------------
-- | User Put
-------------------------------------------------------------------------------

{-@ userPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userPut :: Controller ()
userPut = do
  (PutReq (InvitationCode id code) UserData {..}) <- decodeBody
  let user = mkUser emailAddress password fullName displayName affiliation "attendee"
  _ <- selectFirstOr
    (errorResponse status403 (Just "invalid invitation"))
    (   (invitationId' ==. id)
    &&: (invitationCode' ==. code)
    &&: (invitationEmailAddress' ==. emailAddress)
    &&: (invitationAccepted' ==. False)
    )
  userId <- insert user
  _      <- updateWhere (invitationId' ==. id) (invitationAccepted' `assign` True)
  respondJSON status201 (object ["id" .= userId])

data PutReq = PutReq
  { putReqInvitationCode :: InvitationCode
  , putReqUser :: UserData
  }
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON (stripPrefix "putReq")

data UserData = UserData
  { emailAddress :: Text
  , password :: Text
  , fullName :: Text
  , displayName :: Text
  , affiliation :: Text
  }
  deriving Generic

instance FromJSON UserData where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON UserData where
  toEncoding = genericToEncoding defaultOptions
