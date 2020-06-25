module SMTP
  ( mkPublicAddress
  , connectSMTPSSLWithSettings
  , sendPlainTextMail
  , authenticate
  , Address
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

{-@
assume sendPlainTextMail :: forall <out :: Entity User -> Bool>.
  Address<out>
  -> Address
  -> String
  -> _
  -> SMTPConnection
  -> TaggedT<{\_ -> True}, out> m ()
@-}
sendPlainTextMail
  :: MonadTIO m => Address -> Address -> String -> LT.Text -> SMTPConnection -> TaggedT m ()
sendPlainTextMail (Address to) (Address from) subject body conn =
  liftTIO $ TIO $ SMTP'.sendPlainTextMail to from subject body conn

authenticate :: MonadTIO m => AuthType -> String -> String -> SMTPConnection -> m Bool
authenticate auth user pass conn = liftTIO $ TIO $ SMTP'.authenticate auth user pass conn
