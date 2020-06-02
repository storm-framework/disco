{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
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
import           Crypto                         ( genRandomCodes )
import           Text.Read                      ( readMaybe )
import           Worker
import           SMTP
import           Exception
import           System.IO.Unsafe               ( unsafePerformIO )

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
  let invitations = map
        (\((InvitationInsert f e), code) -> mkInvitation code f e False "not_sent" Nothing)
        (zip reqData codes)
  ids <- insertMany invitations
  _   <- runWorker (sendEmails ids)
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])

sendEmails :: [InvitationId] -> Worker ()
sendEmails ids = do
  invitations <- selectList (invitationId' <-. ids)
  conn        <- connectSMTP "localhost"
  forMC invitations $ \invitation -> do
    id           <- project invitationId' invitation
    emailAddress <- project invitationEmailAddress' invitation
    code         <- project invitationCode' invitation
    fullName     <- project invitationFullName' invitation
    let from = mkPublicAddress (Just "Binah Team") ("binah@goto.binah.com")
    let to   = mkPublicAddress (Just fullName) emailAddress
    let body = "To join please visit the following link\n" ++ invitationLink id code
    let mail = simpleMail' to from "Invitation" (LT.pack body)
    res <- tryT $ renderAndSend conn mail
    case res of
      Left (SomeException e) -> do
        let up1 = (invitationEmailError' `assign` (Just (show e)))
        let up2 = invitationEmailStatus' `assign` "error"
        updateWhere (invitationId' ==. id) (up1 `combine` up2)
      Right _ -> updateWhere (invitationId' ==. id) (invitationEmailStatus' `assign` "sent")
  return ()


invitationLink :: InvitationId -> Text -> String
invitationLink id code = "http://localhost:8000/invitation?code=" ++ sid ++ "." ++ (T.unpack code)
  where sid = show (fromSqlKey id)


newtype PutReq = PutReq [InvitationInsert]
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | Invitation Get
--------------------------------------------------------------------------------

{-@ invitationGet :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationGet :: Int64 -> Controller ()
invitationGet iid = do
  let id = toSqlKey iid :: InvitationId
  code <- listToMaybe <$> queryParams "code"
  case code of
    Nothing   -> respondError status400 (Just "missing code")
    Just code -> do
      invitation <- selectFirstOr
        notFoundJSON
        (invitationCode' ==. code &&: invitationId' ==. id &&: invitationAccepted' ==. False)
      res <-
        InvitationData
        <$> project invitationId'           invitation
        <*> project invitationFullName'     invitation
        <*> project invitationEmailAddress' invitation
        <*> project invitationAccepted'     invitation
      respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Index
--------------------------------------------------------------------------------

{-@ invitationIndex :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationIndex :: Controller ()
invitationIndex = do
  viewer      <- requireAuthUser
  _           <- requireOrganizer viewer
  invitations <- selectList trueF
  res         <- forMC invitations $ \invitation ->
    do
        InvitationData
      <$> project invitationId'           invitation
      <*> project invitationFullName'     invitation
      <*> project invitationEmailAddress' invitation
      <*> project invitationAccepted'     invitation
  respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Data
--------------------------------------------------------------------------------

data InvitationInsert = InvitationInsert
  { insertFullName     :: Text
  , insertEmailAddress :: Text
  }
  deriving Generic

instance FromJSON InvitationInsert where
  parseJSON = genericParseJSON (stripPrefix "insert")

data InvitationData = InvitationData
  { invitationId :: InvitationId
  , invitationFullName :: Text
  , invitationEmailAddress :: Text
  , invitationAccepted :: Bool
  }
  deriving Generic

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
