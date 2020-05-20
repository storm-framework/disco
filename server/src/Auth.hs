{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Control.Monad.Trans.Class      ( lift )
import           JSON
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
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

import           Controller

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

{-@ signIn :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
signIn :: Controller ()
signIn = do
    req                  <- request
    body                 <- liftTIO $ reqBody req
    (username, password) <- case decode body of
        Nothing                            -> respond400 Nothing
        Just (SignInReq username password) -> return (username, password)
    user    <- authUser username password
    userRes <-
        UserRes username
        <$> project userFullName'     user
        <*> project userAffiliation'  user
        <*> project userEmailAddress' user
        <*> project userLevel'        user
    respond200 $ SignInRes { resToken = "", resUser = userRes }

{-@ ignore authUser @-}
{-@
assume authUser ::
  _ -> _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ {u: (Entity User) | u == currentUser}
@-}
authUser :: Text -> Text -> Controller (Entity User)
authUser username password = do
    maybeUser <- selectFirst (userPassword' ==. password &&: userUsername' ==. username)
    case maybeUser of
        Nothing   -> respond401 (Just "incorrect login")
        Just user -> return user
