{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Data.Aeson
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Time             ( MonadTime(..) )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad                  ( replicateM
                                                , when
                                                )
import           Control.Lens.Operators         ( (?~)
                                                , (^.)
                                                , (^?)
                                                , (<&>)
                                                )
import           Control.Lens.Combinators
                                         hiding ( assign )
import           Control.Lens.Lens              ( (&) )
import           Control.Lens.Internal.ByteString
                                                ( unpackLazy8 )
import           Frankie.Auth
import           Crypto.JWT
import           Crypto.JOSE.Types              ( Base64Octets(..) )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy.Encoding       as L
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.ByteString.Lazy          as L
import           Data.Int                       ( Int64 )
import           Data.Maybe                     ( listToMaybe )
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           GHC.Generics
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
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
import           Controllers.User               ( extractUserData
                                                , UserData
                                                )
import           Model
import           JSON
import           Crypto
import           AWS
import           Network.AWS.S3
import           Network.AWS.Data.Text          ( toText )


{-@ ignore addOrganizer @-}
addOrganizer :: UserCreate -> Task UserId
addOrganizer UserCreate {..} = do
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 password))
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
  insert user

--------------------------------------------------------------------------------
-- | SignIn
--------------------------------------------------------------------------------

{-@ signIn :: TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
signIn :: Controller ()
signIn = do
  (SignInReq emailAddress password) <- decodeBody
  user                              <- authUser emailAddress password
  userId                            <- project userId' user
  token                             <- genJwt userId
  userData                          <- extractUserData user

  respondJSON status200 $ AuthRes (unpackLazy8 token) userData

{-@ ignore authUser @-}
{-@ authUser :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
authUser :: Text -> Text -> Controller (Entity User)
authUser emailAddress password = do
  user <- selectFirstOr (errorResponse status401 (Just "Incorrect credentials"))
                        (userEmailAddress' ==. emailAddress)
  encrypted <- project userPassword' user
  if verifyPass' (Pass (T.encodeUtf8 password)) (EncryptedPass encrypted)
    then return user
    else respondError status401 (Just "Incorrect credentials")

data SignInReq = SignInReq
  { signInReqEmailAddress :: Text
  , signInReqPassword :: Text
  }
  deriving Generic

instance FromJSON SignInReq where
  parseJSON = genericParseJSON (stripPrefix "signInReq")

data AuthRes = AuthRes
  { authResAccessToken :: String
  , authResUser :: UserData
  }
  deriving Generic

instance ToJSON AuthRes where
  toEncoding = genericToEncoding (stripPrefix "authRes")

-------------------------------------------------------------------------------
-- | SignUp
-------------------------------------------------------------------------------

{-@ signUp :: TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
signUp :: Controller ()
signUp = do
  (SignUpReq (InvitationCode id code) uc@UserCreate {..}) <- decodeBody
  validateUser uc
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 password))
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
  token    <- genJwt userId
  userData <- extractUserData user
  respondJSON status201 $ AuthRes (unpackLazy8 token) userData

validateUser :: UserCreate -> Controller ()
validateUser UserCreate {..} = do
  when (T.length emailAddress == 0) $ respondError status400 (Just "missing email address")
  when (T.length password == 0) $ respondError status400 (Just "missing password")
  when (T.length displayName == 0) $ respondError status400 (Just "missing displayName")
  when (T.length bio > 300) $ respondError status400 (Just "bio too long")

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


data SignUpReq = SignUpReq
  { signUpReqInvitationCode :: InvitationCode
  , signUpReqUser :: UserCreate
  }
  deriving Generic

instance FromJSON SignUpReq where
  parseJSON = genericParseJSON (stripPrefix "signUpReq")

data UserCreate = UserCreate
  { emailAddress :: Text
  , password :: Text
  , photoURL :: Maybe Text
  , displayName :: Text
  , institution :: Text
  , pronouns :: Text
  , website :: Text
  , bio :: Text
  }
  deriving Generic

instance FromJSON UserCreate where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | SignIn
--------------------------------------------------------------------------------

{-@ presignS3URL :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
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
  t              <- liftTIO currentTime
  AWSConfig {..} <- configAWS <$> getConfig
  let objectKey = textBase64 emailAddress
  let request   = putObject awsBucket (ObjectKey objectKey) ""
  signedUrl <- presignURL awsAuth awsRegion t 900 request
  respondJSON status200 $ T.decodeUtf8 signedUrl

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
                          Nothing   -> respondError status401 Nothing
  }

{-@ ignore checkIfAuth @-}
checkIfAuth :: Controller (Maybe (Entity User))
checkIfAuth = do
  key        <- configSecretKey <$> getConfig
  authHeader <- requestHeader hAuthorization
  let token = authHeader >>= ByteString.stripPrefix "Bearer " <&> L.fromStrict
  claims <- liftTIO $ mapM (doVerify key) token
  case claims of
    Just (Right claims) -> do
      let sub    = claims ^. claimSub ^? _Just . string
      let userId = sub <&> T.unpack >>= readMaybe <&> toSqlKey
      case userId of
        Nothing     -> return Nothing
        Just userId -> selectFirst (userId' ==. userId)
    _ -> return Nothing

-------------------------------------------------------------------------------
-- | JWT
-------------------------------------------------------------------------------

{-@ genJwt :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
genJwt :: UserId -> Controller L.ByteString
genJwt userId = do
  key    <- configSecretKey `fmap` getConfig
  claims <- liftTIO $ mkClaims userId
  jwt    <- liftTIO $ doJwtSign key claims
  case jwt of
    Right jwt                         -> return (encodeCompact jwt)
    Left  (JWSError                e) -> respondError status500 (Just (show e))
    Left  (JWTClaimsSetDecodeError s) -> respondError status400 (Just s)
    Left  JWTExpired                  -> respondError status401 (Just "expired token")
    Left  _                           -> respondError status401 Nothing


mkClaims :: UserId -> TIO ClaimsSet
mkClaims userId = do
  t <- currentTime
  return $ emptyClaimsSet & (claimSub ?~ uid ^. re string) & (claimIat ?~ NumericDate t)
  where uid = T.pack (show (fromSqlKey userId))

doJwtSign :: JWK -> ClaimsSet -> TIO (Either JWTError SignedJWT)
doJwtSign key claims = runExceptT $ do
  alg <- bestJWSAlg key
  signClaims key (newJWSHeader ((), alg)) claims

doVerify :: JWK -> L.ByteString -> TIO (Either JWTError ClaimsSet)
doVerify key s = runExceptT $ do
  let audCheck = const True
  s' <- decodeCompact s
  verifyClaims (defaultJWTValidationSettings audCheck) key s'
