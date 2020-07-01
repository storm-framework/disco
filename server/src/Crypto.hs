module Crypto
  ( encryptPassTIO'
  , module Crypto.Scrypt
  )
where

import           Control.Monad.Time             ( MonadTime(..) )
import           Crypto.JWT                     ( MonadRandom(..) )
import           Crypto.Scrypt
import           Data.Text                      ( Text(..) )
import qualified Data.Text.Encoding            as T
import           Control.Monad                  ( replicateM )

import           Controllers
import           Binah.Infrastructure
import           Binah.Filters
import           Model

instance MonadRandom TIO where
  getRandomBytes x = TIO (getRandomBytes x)

instance MonadTime TIO where
  currentTime = TIO currentTime

----------------------------------------------------------------------------------------------------
-- | Scrypto
----------------------------------------------------------------------------------------------------

encryptPassTIO' :: MonadTIO m => Pass -> m EncryptedPass
encryptPassTIO' = liftTIO . TIO . encryptPassIO'
