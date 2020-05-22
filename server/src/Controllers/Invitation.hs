{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Data.Aeson.Types
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
import           Auth                           ( genRandomCodes )
import           Text.Read                      ( readMaybe )

--------------------------------------------------------------------------------
-- | Invitation Put (create invitations)
--------------------------------------------------------------------------------

{-@ invitationPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationPut :: Controller ()
invitationPut = do
  viewer           <- requireAuthUser
  _                <- requireOrganizer viewer
  (PutReq reqData) <- decodeBody
  codes            <- genRandomCodes (length reqData)
  let invitations =
        map (\((InvitationData f e), code) -> mkInvitation code f e False) (zip reqData codes)
  ids <- insertMany invitations
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])

newtype PutReq = PutReq [InvitationData]
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | Invitation Get
--------------------------------------------------------------------------------

{-@ invitationGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationGet :: Controller ()
invitationGet = do
  code <- listToMaybe <$> queryParams "code"
  case code >>= parseCode of
    Nothing                       -> respondError status400 (Just "missing code")
    Just (InvitationCode id code) -> do
      invitation <- selectFirstOr
        notFoundJSON
        (invitationCode' ==. code &&: invitationId' ==. id &&: invitationAccepted' ==. False)
      res <-
        InvitationData
        <$> project invitationFullName'     invitation
        <*> project invitationEmailAddress' invitation
      respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Data
--------------------------------------------------------------------------------

data InvitationData = InvitationData
  { invitationFullName     :: Text
  , invitationEmailAddress :: Text
  }
  deriving Generic

instance FromJSON InvitationData where
  parseJSON = genericParseJSON (stripPrefix "invitation")

instance ToJSON InvitationData where
  toEncoding = genericToEncoding (stripPrefix "invitation")

data InvitationCode = InvitationCode InvitationId Text deriving Generic

instance FromJSON InvitationCode where
  parseJSON = withText "InvitationCode" parse
   where
    parse t = case parseCode t of
      Nothing -> fail "Invalid invitation id"
      Just id -> return id

parseCode :: Text -> Maybe InvitationCode
parseCode text = case readMaybe (T.unpack h) of
  Nothing -> Nothing
  Just id -> Just $ InvitationCode (toSqlKey id) (T.drop 1 t)
  where (h, t) = T.breakOn "." text
