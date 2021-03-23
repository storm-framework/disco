{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Sync where

import           Data.Maybe
import           GHC.Generics
import           Control.Monad.Time             ( MonadTime(..) )

import           Storm.Core
import           Storm.Actions
import           Storm.Updates
import           Storm.Insert
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Storm.Time
import           Storm.JSON

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
