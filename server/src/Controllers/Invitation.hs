{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Data.Aeson.Types
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
import           GHC.Generics
import           Text.Mustache                  ( (~>) )
import qualified Text.Mustache.Types           as Mustache
import qualified Network.Mail.Mime             as M
import           Frankie.Config

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
  _                <- checkOrganizer viewer
  (PutReq reqData) <- decodeBody
  codes            <- genRandomCodes (length reqData)
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
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])

data EmailData = EmailData
  { emailDataInvitationId :: InvitationId
  , emailDataInvitationCode :: Text
  }

instance TemplateData EmailData where
  templateFile = "invitation.json.mustache"
  toMustache (EmailData id code) = Mustache.object ["invitationId" ~> id, "invitationCode" ~> code]

data EmailRender = EmailRender
  { emailRenderBodyPlain :: LT.Text
  , emailRenderSubject :: String
  , emailRenderFrom :: String
  }
  deriving (Generic, Show)

instance FromJSON EmailRender where
  parseJSON = genericParseJSON (stripPrefix "emailRender")

sendEmails :: [InvitationId] -> Task ()
sendEmails ids = do
  SMTPConfig {..} <- configSMTP <$> getConfig
  invitations     <- selectList (invitationId' <-. ids)
  let settings = defaultSettingsSMTPSSL { sslPort = smtpPort }
  conn <- connectSMTPSSLWithSettings smtpHost settings
  _    <- authenticate LOGIN smtpUser smtpPass conn
  mapMC (sendEmail' conn) invitations
  return ()

sendEmail :: Int64 -> Task ()
sendEmail iid = do
  let invitationId = toSqlKey iid
  SMTPConfig {..} <- configSMTP <$> getConfig
  invitation      <- selectFirst (invitationId' ==. invitationId)
  case invitation of
    Just invitation -> do
      let settings = defaultSettingsSMTPSSL { sslPort = smtpPort }
      conn <- connectSMTPSSLWithSettings smtpHost settings
      _    <- authenticate LOGIN smtpUser smtpPass conn
      sendEmail' conn invitation
    Nothing -> return ()

sendEmail' :: SMTPConnection -> Entity Invitation -> Task ()
sendEmail' conn invitation = do
  id                        <- project invitationId' invitation
  (to, from, subject, body) <- renderEmail invitation
  res                       <- tryT $ sendPlainTextMail to from subject body conn
  case res of
    Left (SomeException e) -> do
      let up1 = invitationEmailError' `assign` Just (show e)
      let up2 = invitationEmailStatus' `assign` "error"
      updateWhere (invitationId' ==. id) (up1 `combine` up2)
    Right _ -> updateWhere (invitationId' ==. id) (invitationEmailStatus' `assign` "sent")

renderEmail :: Entity Invitation -> Task (Address, Address, String, LT.Text)
renderEmail invitation = do
  id           <- project invitationId' invitation
  emailAddress <- project invitationEmailAddress' invitation
  code         <- project invitationCode' invitation
  raw          <- renderTemplate (EmailData id code)
  let EmailRender {..} = fromJust $ decode (LT.encodeUtf8 (LT.fromStrict raw))
  let to               = mkPublicAddress (T.unpack emailAddress)
  let from             = mkPublicAddress emailRenderFrom
  return (to, from, emailRenderSubject, emailRenderBodyPlain)


newtype PutReq = PutReq [InvitationInsert]
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | Invitation Get
--------------------------------------------------------------------------------

{-@ invitationGet :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
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

{-@ invitationList :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationList :: Controller ()
invitationList = do
  viewer      <- requireAuthUser
  _           <- checkOrganizer viewer
  invitations <- selectList trueF
  res         <- mapMC extractInvitationData invitations
  respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Data
--------------------------------------------------------------------------------

data InvitationInsert = InvitationInsert
  { insertEmailAddress :: Text
  , insertFirstName    :: Text
  , insertLastName     :: Text
  , insertInstitution  :: Text
  }
  deriving Generic

instance FromJSON InvitationInsert where
  parseJSON = genericParseJSON (stripPrefix "insert")

data InvitationData = InvitationData
  { invitationId           :: InvitationId
  , invitationEmailAddress :: Text
  , invitationFirstName    :: Text
  , invitationLastName     :: Text
  , invitationInstitution  :: Text
  , invitationAccepted     :: Bool
  }
  deriving Generic

extractInvitationData :: Entity Invitation -> Controller InvitationData
extractInvitationData invitation =
  InvitationData
    <$> project invitationId'           invitation
    <*> project invitationEmailAddress' invitation
    <*> project invitationFirstName'    invitation
    <*> project invitationLastName'     invitation
    <*> project invitationInstitution'  invitation
    <*> project invitationAccepted'     invitation

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
