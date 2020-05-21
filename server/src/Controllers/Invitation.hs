{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

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

import           Controllers
import           Model
import           JSON

--------------------------------------------------------------------------------
-- | Invitation Put (create invitations)
--------------------------------------------------------------------------------

{-@ invitationPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationPut :: Controller ()
invitationPut = do
  viewer                     <- requireAuthUser
  _                          <- requireOrganizer viewer
  (InvitationPutReq reqData) <- decodeBody
  let invitations = map (\(InvitationData f e) -> mkInvitation "code" f e False) reqData
  ids <- insertMany invitations
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])

newtype InvitationPutReq = InvitationPutReq [InvitationData]
  deriving Generic

instance FromJSON InvitationPutReq where
  parseJSON = genericParseJSON defaultOptions

---------------------------------------------------------------------------------
-- | Invitation Get
---------------------------------------------------------------------------------

{-@ invitationGet :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationGet :: Int64 -> Controller ()
invitationGet iid = do
  let invitationId = toSqlKey iid
  code <- listToMaybe <$> queryParams "code"
  case code of
    Nothing   -> respondError status400 (Just "missing code")
    Just code -> do
      invitation <- selectFirstOr notFoundJSON
                                  (invitationId' ==. invitationId &&: invitationCode' ==. code)
      res <-
        InvitationData
        <$> project invitationFullName'     invitation
        <*> project invitationEmailAddress' invitation
      respondJSON status200 res

---------------------------------------------------------------------------------------------------
-- | Invitation Data
---------------------------------------------------------------------------------------------------

data InvitationData = InvitationData
  { invitationFullName     :: Text
  , invitationEmailAddress :: Text
  }
  deriving Generic

instance FromJSON InvitationData where
  parseJSON = genericParseJSON (stripPrefix "invitation")

instance ToJSON InvitationData where
  toEncoding = genericToEncoding (stripPrefix "invitation")
