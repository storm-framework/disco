{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}
{-@ LIQUID "--compile-spec"      @-}

module Auth where

import           Control.Monad.Time             ( MonadTime(..) )
import qualified Crypto.Hash                   as Crypto
import qualified Crypto.MAC.HMAC               as Crypto
import qualified Data.ByteArray                as BA
import           Frankie.Auth
import qualified Frankie.Log                   as Log
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as Char8
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.ByteString.Lazy          as L
import Data.Maybe ( listToMaybe )
-- import qualified Data.Time.Format              as Time
import           Data.Time.Clock                ( UTCTime
                                                , secondsToDiffTime
                                                )
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           GHC.Generics
import           Text.Read                      ( readMaybe )
import           Frankie.Cookie

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
-- import           Binah.Templates
import           Binah.Frankie
import           Binah.Crypto
import           Binah.JSON

import           Controllers
import           Controllers.User               ( extractUserData
                                                -- , UserData
                                                )
import           Model
import           JSON
import           AWS
import           Network.AWS.S3

{-@ ignore addOrganizer @-}
addOrganizer :: UserCreate -> Task UserId
addOrganizer UserCreate {..} = do
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 password))
  t                       <- liftTIO currentTime
  let user = mkUser emailAddress
                    encrypted
                    photoURL
                    displayName
                    institution
                    pronouns
                    website
                    bio
                    "organizer"
                    "public"
                    Nothing
                    False
                    t
  insert user

--------------------------------------------------------------------------------
-- | SignOut
--------------------------------------------------------------------------------

{-@ signOut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ () @-}
signOut :: Controller ()
signOut = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- updateWhere (userId' ==. viewerId)
                          ((userRoom' `assign` Nothing) `combine` (userActive' `assign` False))
  respondTagged $ expireSessionCookie (emptyResponse status201)

--------------------------------------------------------------------------------
-- | SignIn
--------------------------------------------------------------------------------

{-@ signIn :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ () @-}
signIn :: Controller ()
signIn = do
  (SignInReq emailAddress password) <- decodeBody
  user                              <- authUser emailAddress password
  userId                            <- project userId' user
  token                             <- genToken userId
  userData                          <- extractUserData user

  respondTagged $ setSessionCookie token (jsonResponse status201 userData)

{-@ ignore authUser @-}
{-@ authUser :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ _ @-}
authUser :: T.Text -> T.Text -> Controller (Entity User)
authUser emailAddress password = do
  user <- selectFirstOr (errorResponse status401 (Just "Incorrect credentials"))
                        (userEmailAddress' ==. emailAddress)
  encrypted <- project userPassword' user
  if verifyPass' (Pass (T.encodeUtf8 password)) (EncryptedPass encrypted)
    then return user
    else respondError status401 (Just "Incorrect credentials")

data SignInReq = SignInReq
  { signInReqEmailAddress :: T.Text
  , signInReqPassword :: T.Text
  }
  deriving Generic

instance FromJSON SignInReq where
  parseJSON = genericParseJSON (stripPrefix "signInReq")

-------------------------------------------------------------------------------
-- | SignUp
-------------------------------------------------------------------------------
{-@ ignore signUp @-}
{-@ signUp :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ () @-}
signUp :: Controller ()
signUp = do
  (SignUpReq (InvitationCode id code) uc@UserCreate {..}) <- decodeBody
  validateUser uc
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 password))
  t                       <- liftTIO currentTime
  let user = mkUser emailAddress
                    encrypted
                    photoURL
                    displayName
                    institution
                    pronouns
                    website
                    bio
                    "attendee"
                    "public"
                    Nothing
                    True
                    t
  _ <- selectFirstOr
    (errorResponse status403 (Just "invalid invitation"))
    (   (invitationId' ==. id)
    &&: (invitationCode' ==. code)
    &&: (invitationEmailAddress' ==. emailAddress)
    &&: (invitationAccepted' ==. False)
    )
  userId   <- insert user
  _        <- updateWhere (invitationId' ==. id) (invitationAccepted' `assign` True)
  user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  token    <- genToken userId
  userData <- extractUserData user
  respondTagged $ setSessionCookie token (jsonResponse status201 userData)

validateUser :: UserCreate -> Controller ()
validateUser UserCreate {..} = do
  whenT (T.length emailAddress == 0) $ respondError status400 (Just "missing email address")
  whenT (T.length password == 0) $ respondError status400 (Just "missing password")
  whenT (T.length displayName == 0) $ respondError status400 (Just "missing displayName")
  whenT (T.length bio > 300) $ respondError status400 (Just "bio too long")

data InvitationCode = InvitationCode InvitationId T.Text deriving Generic

instance FromJSON InvitationCode where
  parseJSON = withText "InvitationCode" parse
   where
    parse t = case parseCode t of
      Nothing -> fail "Invalid invitation id"
      Just id -> return id

parseCode :: T.Text -> Maybe InvitationCode
parseCode text = case readMaybe (T.unpack h) of
  Nothing -> Nothing
  Just id -> Just $ InvitationCode (toSqlKey id) (T.drop 1 t)
  where (h, t) = T.breakOn "." text


data SignUpReq = SignUpReq
  { signUpReqInvitationCode :: InvitationCode
  , signUpReqUser :: UserCreate
  }
  deriving Generic

instance FromJSON SignUpReq where
  parseJSON = genericParseJSON (stripPrefix "signUpReq")

data UserCreate = UserCreate
  { emailAddress :: T.Text
  , password     :: T.Text
  , photoURL     :: Maybe T.Text
  , displayName  :: T.Text
  , institution  :: T.Text
  , pronouns     :: T.Text
  , website      :: T.Text
  , bio          :: T.Text
  }
  deriving Generic

instance FromJSON UserCreate where
  parseJSON = genericParseJSON defaultOptions

expireSessionCookie :: Response -> Response
expireSessionCookie =
  setCookie (defaultSetCookie { setCookieName = "session", setCookieMaxAge = Just 0 })

setSessionCookie :: SessionToken -> Response -> Response
setSessionCookie token = setCookie
  (defaultSetCookie { setCookieName   = "session"
                    , setCookieValue  = L.toStrict (encode token)
                    , setCookieMaxAge = Just $ secondsToDiffTime 604800 -- 1 week
                    }
  )

--------------------------------------------------------------------------------
-- | presignS3URL
--------------------------------------------------------------------------------

{-@ ignore photoPost @-}
photoPost :: T.Text -> Controller ()
photoPost id = do
  image   <- photoImage
  photoMb <- selectFirst (photoHash' ==. id)
  case photoMb of
    Just _  -> updatePhoto id image
    Nothing -> insertPhoto id image 
  respondJSON status200 $ "saved photo for: " <> id

photoImage :: Controller (FileInfo L.ByteString)
photoImage = do 
  (_, files) <- decodeFiles
  case files of
    ("image", f) : _ -> return f
    _                -> respondError status401 (Just "Invalid photo POST")

updatePhoto :: T.Text -> FileInfo L.ByteString -> Controller ()
updatePhoto id image = do 
  logT Log.INFO ("updatePhoto: id " ++ show id ++ " " ++ show (fileName image))
  updateWhere (photoHash' ==. id)
    ((photoFileName'    `assign` (fileName        image)) `combine` 
     (photoFileType'    `assign` (fileContentType image)) `combine` 
     (photoFileContent' `assign` L.toStrict (fileContent image))
    )

insertPhoto :: T.Text -> FileInfo L.ByteString -> Controller () 
insertPhoto id image = do 
  let name  = fileName image
  let typ   = fileContentType image
  let blob  = L.toStrict (fileContent image)
  let photo = mkPhoto id name typ blob 
  key      <- insert photo
  logT Log.INFO ("insertPhoto: id " ++ show (id, fileName image, key))

{-@ ignore photoGet @-}
photoGet :: T.Text -> Controller ()
photoGet id = do
  logT Log.INFO ("photoGet: id " ++ show id)
  photo <- selectFirstOr (errorResponse status401 Nothing) (photoHash' ==. id)
  typ   <- project photoFileType'    photo 
  blob  <- project photoFileContent' photo
  name  <- project photoFileName'    photo
  logT Log.INFO ("photoGet-respond: " ++ show (id, name))
  respondFile status200 typ (L.fromStrict blob) 

{-@ ignore presignS3URL @-}
{-@ presignS3URL :: TaggedT <{\_ -> False}, {\_ -> True}> _ _ () @-}
presignS3URL :: Controller ()
presignS3URL = do
  code         <- listToMaybe <$> queryParams "code"
  invitationId <- listToMaybe <$> queryParams "id"
  emailAddress <- case (code, invitationId) of
    (Just code, Just id) -> do
      invitation <- selectFirstOr (errorResponse status401 Nothing)
                                  (invitationId' ==. id &&: invitationCode' ==. code)
      project invitationEmailAddress' invitation
    _ -> requireAuthUser >>= project userEmailAddress'
  let signedUrl = makePhotoURL emailAddress
  -- t              <- liftTIO currentTime
  -- AWSConfig {..} <- configAWS `fmap` getConfigT
  -- let objectKey = textBase64 emailAddress
  -- let request   = putObject awsBucket (ObjectKey objectKey) ""
  -- signedUrl <- T.decodeUtf8 <$> presignURL awsAuth awsRegion t 900 request
  respondJSON status200 signedUrl

makePhotoURL :: T.Text -> T.Text
makePhotoURL email = "http://localhost:3000/api/photo/" <> textBase64 email

textBase64 :: T.Text -> T.Text
textBase64 = T.decodeUtf8 . B64Url.encode . T.encodeUtf8

-------------------------------------------------------------------------------
-- | Auth method
-------------------------------------------------------------------------------

authMethod :: AuthMethod (Entity User) Controller
authMethod = AuthMethod
  { authMethodTry     = checkIfAuth
  , authMethodRequire = checkIfAuth >>= \case
                          Just user -> pure user
                          Nothing   -> respondTagged $ expireSessionCookie (emptyResponse status401)
  }

{-@ ignore checkIfAuth @-}
checkIfAuth :: Controller (Maybe (Entity User))
checkIfAuth = do
  cookie  <- listToMaybe <$> getCookie "session"
  case cookie >>= decode . L.fromStrict of
    Just t@SessionToken{..} -> do
      valid <- verifyToken t
      if valid then selectFirst (userId' ==. stUserId) else return Nothing
    _ -> return Nothing

-------------------------------------------------------------------------------
-- | Session Tokens
-------------------------------------------------------------------------------

data SessionToken = SessionToken
  { stUserId :: UserId
  , stTime   :: UTCTime
  , stHash   :: T.Text
  }
  deriving Generic

instance ToJSON SessionToken where
  toEncoding = genericToEncoding (stripPrefix "st")

instance FromJSON SessionToken where
  parseJSON = genericParseJSON (stripPrefix "st")

{-@ ignore genToken @-}
genToken :: UserId -> Controller SessionToken
genToken userId = do
  key  <- configSecretKey `fmap` getConfigT
  time <- currentTime
  return (SessionToken userId time (doHmac key userId time))

verifyToken :: SessionToken -> Controller Bool
verifyToken SessionToken{..} = do
  key <- configSecretKey `fmap` getConfigT
  return (doHmac key stUserId stTime == stHash)

doHmac :: BS.ByteString -> UserId -> UTCTime -> T.Text
doHmac key userId time = T.decodeUtf8 . B64Url.encode . BS.pack $ h
  where
    t   = show time
    u   = show (fromSqlKey userId)
    msg = Char8.pack (t ++ ":" ++ u)
    h   = BA.unpack (hs256 key msg)

hs256 :: BS.ByteString -> BS.ByteString -> Crypto.HMAC Crypto.SHA256
hs256 = Crypto.hmac