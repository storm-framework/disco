{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Control.Monad.Random
import           Database.Persist.Sql           ( toSqlKey )
import           GHC.Generics
import           Text.Mustache                  ( (~>) )
import qualified Text.Mustache.Types           as Mustache
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64.URL    as B64Url

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie
import           Binah.SMTP
import           Binah.Random ()
import           Binah.JSON

import           Controllers
import           Model
import           JSON
-- import           Text.Read                      ( readMaybe )

--------------------------------------------------------------------------------
-- | Invitation Put (create invitations)
--------------------------------------------------------------------------------

{-@ invitationPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
invitationPut :: Controller ()
invitationPut = do
  viewer           <- requireAuthUser
  _                <- checkOrganizer viewer
  (PutReq reqData) <- decodeBody
  codes            <- replicateT (length reqData) genRandomCode
  let invitations = zipWith
        (\InvitationInsert {..} code -> mkInvitation code
                                                     insertEmailAddress
                                                     insertFirstName
                                                     insertLastName
                                                     insertInstitution
                                                     False
                                                     "not_sent"
                                                     Nothing
        )
        reqData
        codes
  ids <- insertMany invitations
  _   <- runTask (sendEmails ids)
  respondJSON status201 (object ["keys" .= ids])

{-@ genRandomCode :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
genRandomCode :: Controller T.Text
genRandomCode = do
  bytes <- liftTIO getRandoms
  return $ T.decodeUtf8 $ B64Url.encode $ BS.pack (take 24 bytes)


data EmailData = EmailData
  { emailDataInvitationId :: InvitationId
  , emailDataInvitationCode :: T.Text
  }

instance TemplateData EmailData where
  templateFile = "invitation.json.mustache"
  toMustache (EmailData id code) = Mustache.object ["invitationId" ~> id, "invitationCode" ~> code]

data EmailRender = EmailRender
  { emailRenderBodyPlain :: LT.Text
  , emailRenderSubject :: T.Text
  , emailRenderFrom :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON EmailRender where
  parseJSON = genericParseJSON (stripPrefix "emailRender")

{-@ sendEmails :: _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ () @-}
sendEmails :: [InvitationId] -> Task ()
sendEmails ids = do
  SMTPConfig {..} <- configSMTP `fmap` getConfigT
  invitations     <- selectList (invitationId' <-. ids)
  conn            <- connectSMTPS' smtpHost smtpPort
  _               <- login conn smtpUser smtpPass
  mapT (sendEmail' conn) invitations
  return ()

{-@ sendEmail :: _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ () @-}
sendEmail :: Int64 -> Task ()
sendEmail iid = do
  let invitationId = toSqlKey iid
  SMTPConfig {..} <- configSMTP `fmap` getConfigT
  invitation      <- selectFirst (invitationId' ==. invitationId)
  case invitation of
    Just invitation -> do
      conn <- connectSMTPS' smtpHost smtpPort
      _    <- login conn smtpHost smtpPass
      sendEmail' conn invitation
    Nothing -> return ()

{-@ sendEmail' :: _ -> _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ () @-}
sendEmail' :: SMTPConnection -> Entity Invitation -> Task ()
sendEmail' conn invitation = do
  id                        <- project invitationId' invitation
  (to, from, subject, body) <- renderEmail invitation
  res                       <- renderAndSend conn (simpleMail' to from subject body)
  case res of
    Left err -> do
      let up =
            (invitationEmailError' `assign` Just (show err))
              `combine` (invitationEmailStatus' `assign` "error")
      updateWhere (invitationId' ==. id) up
    Right _ -> updateWhere (invitationId' ==. id) (invitationEmailStatus' `assign` "sent")

{-@ renderEmail :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
renderEmail :: Entity Invitation
            -> Task (Address (Entity User), Address (Entity User), T.Text, LT.Text)
renderEmail invitation = do
  id           <- project invitationId' invitation
  emailAddress <- project invitationEmailAddress' invitation
  code         <- project invitationCode' invitation
  raw          <- renderTemplate (EmailData id code)
  let EmailRender {..} = fromJust $ decode (LT.encodeUtf8 (LT.fromStrict raw))
  let to               = publicAddress emailAddress
  let from             = publicAddress emailRenderFrom
  return (to, from, emailRenderSubject, emailRenderBodyPlain)


newtype PutReq = PutReq [InvitationInsert]
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | Invitation Get
--------------------------------------------------------------------------------

{-@ invitationGet :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
invitationGet :: InvitationId -> Controller ()
invitationGet id = do
  code <- listToMaybe <$> queryParams "code"
  case code of
    Nothing   -> respondError status400 (Just "missing code")
    Just code -> do
      invitation <- selectFirstOr
        notFoundJSON
        (invitationCode' ==. code &&: invitationId' ==. id &&: invitationAccepted' ==. False)
      res <- extractInvitationData invitation
      respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation List
--------------------------------------------------------------------------------

{-@ invitationList :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
invitationList :: Controller ()
invitationList = do
  viewer      <- requireAuthUser
  _           <- checkOrganizer viewer
  invitations <- selectList trueF
  res         <- mapT extractInvitationData invitations
  respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Data
--------------------------------------------------------------------------------

data InvitationInsert = InvitationInsert
  { insertEmailAddress :: T.Text
  , insertFirstName    :: T.Text
  , insertLastName     :: T.Text
  , insertInstitution  :: T.Text
  }
  deriving Generic

instance FromJSON InvitationInsert where
  parseJSON = genericParseJSON (stripPrefix "insert")

data InvitationData = InvitationData
  { invitationId           :: InvitationId
  , invitationEmailAddress :: T.Text
  , invitationFirstName    :: T.Text
  , invitationLastName     :: T.Text
  , invitationInstitution  :: T.Text
  , invitationAccepted     :: Bool
  }
  deriving Generic

{-@ extractInvitationData :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
extractInvitationData :: Entity Invitation -> Controller InvitationData
extractInvitationData invitation =
  InvitationData
    `fmap` project invitationId'           invitation
    <*>    project invitationEmailAddress' invitation
    <*>    project invitationFirstName'    invitation
    <*>    project invitationLastName'     invitation
    <*>    project invitationInstitution'  invitation
    <*>    project invitationAccepted'     invitation

instance ToJSON InvitationData where
  toEncoding = genericToEncoding (stripPrefix "invitation")
