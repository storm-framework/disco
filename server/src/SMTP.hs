module SMTP
  ( mkPublicAddress
  , connectSMTPSSLWithSettings
  , sendPlainTextMail
  , authenticate
  , Address
  , SMTPError(..)
  , module Network.HaskellNet.SMTP.SSL
  )
where

import           Network.HaskellNet.SMTP.SSL
                                         hiding ( connectSMTPSSLWithSettings
                                                , sendPlainTextMail
                                                , authenticate
                                                )
import qualified Network.HaskellNet.SMTP.SSL   as SMTP'
import qualified Data.Text.Lazy                as LT

-- Without this import LH goes crazy
import qualified Data.Text                     as T

import           Binah.Infrastructure
import           Binah.Core
import           Binah.Actions
import           Model
import           Control.Exception              ( try
                                                , SomeException(..)
                                                )

newtype SMTPError = SendError String deriving Show

{-@
data Address<out :: Entity User -> Bool> = Address _
@-}
data Address = Address String
{-@ data variance Address covariant @-}

{-@ assume mkPublicAddress :: String -> Address<{\_ -> True}> @-}
mkPublicAddress :: String -> Address
mkPublicAddress = Address

connectSMTPSSLWithSettings :: MonadTIO m => String -> Settings -> m SMTPConnection
connectSMTPSSLWithSettings host settings =
  liftTIO $ TIO $ SMTP'.connectSMTPSSLWithSettings host settings

-- FIXME: The source label shouldn't be True because the exception could contain part of the email's
-- content.
{-@
assume sendPlainTextMail :: forall <out :: Entity User -> Bool>.
  Address<out>
  -> Address
  -> String
  -> _
  -> SMTPConnection
  -> TaggedT<{\_ -> True}, out> m (Either SMTPError ())
@-}
sendPlainTextMail
  :: MonadTIO m
  => Address
  -> Address
  -> String
  -> LT.Text
  -> SMTPConnection
  -> TaggedT m (Either SMTPError ())
sendPlainTextMail (Address to) (Address from) subject body conn = liftTIO $ TIO $ do
  res <- try $ SMTP'.sendPlainTextMail to from subject body conn
  case res of
    Left  (SomeException e) -> return (Left (SendError (show e)))
    Right _                 -> return (Right ())


authenticate :: MonadTIO m => AuthType -> String -> String -> SMTPConnection -> m Bool
authenticate auth user pass conn = liftTIO $ TIO $ SMTP'.authenticate auth user pass conn
