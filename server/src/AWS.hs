module AWS
  ( presignURL
  )
where

import           Network.AWS             hiding ( presignURL )
import qualified Network.AWS.Presign           as Presign

import           Data.ByteString                ( ByteString )
import           Data.Time.Clock                ( UTCTime )

import           Binah.Infrastructure
import           Binah.Core

presignURL
  :: (MonadTIO m, AWSRequest a) => Auth -> Region -> UTCTime -> Seconds -> a -> m ByteString
presignURL auth region time seconds req =
  liftTIO $ TIO $ Presign.presignURL auth region time seconds req
