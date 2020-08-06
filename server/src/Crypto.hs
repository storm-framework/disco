module Crypto
  ( encryptPassTIO'
  , shuffleT
  , module Crypto.Scrypt
  )
where

import           Control.Monad.Time             ( MonadTime(..) )
import qualified Crypto.Random                 as Crypto
import qualified Control.Monad.Random          as Random
import           Crypto.Scrypt
import           Data.Text                      ( Text(..) )
import qualified Data.Text.Encoding            as T
import           Control.Monad                  ( replicateM )

import           Controllers
import           Binah.Infrastructure
import           Binah.Filters
import           Model
import qualified System.Random.Shuffle         as Shuffle

instance Crypto.MonadRandom TIO where
  getRandomBytes x = TIO (Crypto.getRandomBytes x)

instance Random.MonadRandom TIO where
  getRandom   = TIO Random.getRandom
  getRandoms  = TIO Random.getRandoms
  getRandomR  = TIO . Random.getRandomR
  getRandomRs = TIO . Random.getRandomRs

instance MonadTime TIO where
  currentTime = TIO currentTime

{-@ assume shuffleT :: [a] => TaggedT<{\_ -> True}, {\_ -> False}> user m [a] @-}
shuffleT :: MonadTIO m => [a] -> TaggedT user m [a]
shuffleT = liftTIO . Shuffle.shuffleM

----------------------------------------------------------------------------------------------------
-- | Scrypto
----------------------------------------------------------------------------------------------------

encryptPassTIO' :: MonadTIO m => Pass -> m EncryptedPass
encryptPassTIO' = liftTIO . TIO . encryptPassIO'
