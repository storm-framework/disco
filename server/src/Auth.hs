{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Time             ( MonadTime(..) )
import           Control.Monad.Except           ( runExceptT )
import           Control.Lens.Operators         ( (?~)
                                                , (^.)
                                                )
import           Control.Lens.Combinators       ( re )
import           Control.Lens.Lens              ( (&) )
import           Control.Lens.Internal.ByteString
                                                ( unpackLazy8 )
import           Crypto.JWT
import           Crypto.JOSE.Types              ( Base64Octets(..) )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeASCII )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Base64.Lazy   as B64
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
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

import           Model
import           JSON

import           Controllers

data SignInReq = SignInReq
  { reqUsername :: Text
  , reqPassword :: Text
  }
  deriving Generic

instance FromJSON SignInReq where
  parseJSON = genericParseJSON defaultOptions

data UserRes = UserRes
  { userUsername :: Text
  , userFullName :: Text
  , userAffiliation :: Text
  , userEmailAddress :: Text
  , userLevel    :: String
  }
  deriving Generic

data SignInRes = SignInRes
  { resToken :: String
  , resUser  :: UserRes
  }
  deriving Generic

instance ToJSON UserRes where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SignInRes where
  toEncoding = genericToEncoding defaultOptions

{-@ ignore signIn @-}
signIn :: Controller ()
signIn = do
  req                  <- request
  body                 <- liftTIO $ reqBody req
  (username, password) <- case decode body of
    Nothing                            -> respond400 Nothing
    Just (SignInReq username password) -> return (username, password)

  user    <- authUser username password
  userId  <- project userId' user
  token   <- genJwt userId
  userRes <-
    UserRes username
    <$> project userFullName'     user
    <*> project userAffiliation'  user
    <*> project userEmailAddress' user
    <*> project userLevel'        user

  respond200 $ SignInRes { resToken = unpackLazy8 $ encodeCompact token, resUser = userRes }

{-@ ignore genJwt @-}
genJwt :: UserId -> Controller SignedJWT
genJwt userId = do
  claims <- liftTIO $ mkClaims userId
  jwt    <- liftTIO $ doJwtSign claims
  case jwt of
    Right jwt                         -> return jwt
    Left  (JWSError                e) -> respond500 (Just (show e))
    Left  (JWTClaimsSetDecodeError s) -> respond400 (Just s)
    Left  JWTExpired                  -> respond401 (Just "token expired")
    Left  _                           -> respond401 Nothing

{-@ ignore authUser @-}
authUser :: Text -> Text -> Controller (Entity User)
authUser username password = do
  maybeUser <- selectFirst (userPassword' ==. password &&: userUsername' ==. username)
  case maybeUser of
    Nothing   -> respond401 (Just "incorrect login")
    Just user -> return user

---------------------------------------------------------------------------------------------------
-- JWT
---------------------------------------------------------------------------------------------------

mkClaims :: UserId -> TIO ClaimsSet
mkClaims userId = do
  t <- currentTime
  return $ emptyClaimsSet & (claimSub ?~ uid ^. re string) & (claimIat ?~ NumericDate t)
  where uid = T.pack (show (fromSqlKey userId))

doJwtSign :: ClaimsSet -> TIO (Either JWTError SignedJWT)
doJwtSign claims = runExceptT $ do
  alg <- bestJWSAlg key
  signClaims key (newJWSHeader ((), alg)) claims


doJwtVerify :: SignedJWT -> TIO (Either JWTError ClaimsSet)
doJwtVerify jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== "bob")
  verifyClaims config key jwt

key :: JWK
key = fromOctets raw
 where
  raw :: ByteString
  raw = "\xe5L\xb7\xf6\x03|\xb6\n\x10\xd8\xb8\x96\xe2\xc4W@#W\xb4>\th*iiW\x12\x80z\x04i="

instance MonadRandom TIO where
  getRandomBytes x = TIO (getRandomBytes x)

instance MonadTime TIO where
  currentTime = TIO currentTime
