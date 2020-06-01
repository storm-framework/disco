module SMTP
  ( renderAndSend
  , simpleMail'
  , mkPublicAddress
  , connectSMTP
  , Address
  )
where

import qualified Network.Mail.SMTP             as M
import qualified Network.Mail.Mime             as M
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT

import           Binah.Infrastructure
import           Binah.Core
import           Binah.Actions
import           Model


-- TODO: LIQUID TYPES

newtype Mail = Mail M.Mail

newtype Address = Address M.Address

mkPublicAddress :: Maybe T.Text -> T.Text -> Address
mkPublicAddress name email = Address (M.Address name email)

simpleMail' :: Address -> Address -> T.Text -> LT.Text -> Mail
simpleMail' (Address from) (Address to) subject body = Mail $ M.simpleMail' from to subject body

connectSMTP :: MonadTIO m => String -> TaggedT m M.SMTPConnection
connectSMTP hostname = liftTIO $ TIO $ M.connectSMTP hostname

renderAndSend :: MonadTIO m => M.SMTPConnection -> Mail -> TaggedT m ()
renderAndSend conn (Mail mail) = liftTIO $ TIO $ M.renderAndSend conn mail
