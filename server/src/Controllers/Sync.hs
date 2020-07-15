{-# LANGUAGE DeriveGeneric #-}

module Controllers.Sync where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
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

import           Controllers
import           Controllers.User
import           Controllers.Message
import           Controllers.Room
import           Model
import           JSON

----------------------------------------------------------------------------------------------------
-- | Sync
----------------------------------------------------------------------------------------------------

{-@ sync :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
sync :: Controller ()
sync = do
  user     <- requireAuthUser
  userId   <- project userId' user

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

{-@ beacon :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
beacon :: Controller ()
beacon = do
  user   <- requireAuthUser
  userId <- project userId' user
  _      <- updateWhere (userId' ==. userId) (userRoom' `assign` Nothing)
  respondTagged (emptyResponse status200)
