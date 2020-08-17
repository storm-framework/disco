{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Sync where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
import           GHC.Generics
import           Control.Monad.Time             ( MonadTime(..) )

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie
import           Binah.Time

import           Controllers
import           Controllers.User
import           Controllers.Message
import           Controllers.Room
import           Model
import           JSON

----------------------------------------------------------------------------------------------------
-- | Sync
----------------------------------------------------------------------------------------------------

{-@ sync :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
sync :: Controller ()
sync = do
  user   <- requireAuthUser
  userId <- project userId' user
  t      <- liftTIO currentTime
  _      <- updateWhere (userId' ==. userId)
                        ((userLastSync' `assign` t) `combine` (userActive' `assign` True))
  users    <- allUsers
  rooms    <- allRooms
  messages <- messagesFor userId

  respondJSON status200 $ SyncResponse users rooms messages

data SyncResponse = SyncResponse
  { syncResponseUsers :: [UserData]
  , syncResponseRooms :: [RoomData]
  , syncResponseUnreadMessages :: [RecvMessage]
  }
  deriving Generic

instance ToJSON SyncResponse where
  toEncoding = genericToEncoding (stripPrefix "syncResponse")

----------------------------------------------------------------------------------------------------
-- | Beacon
----------------------------------------------------------------------------------------------------

{-@ beacon :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
beacon :: Controller ()
beacon = do
  user   <- requireAuthUser
  userId <- project userId' user
  _      <- updateWhere (userId' ==. userId)
                        ((userRoom' `assign` Nothing) `combine` (userActive' `assign` False))
  respondTagged (emptyResponse status200)
