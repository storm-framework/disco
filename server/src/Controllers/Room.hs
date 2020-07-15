{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}
{-@ LIQUID "--exact-data-con" @-}


module Controllers.Room where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
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
import           Model
import           JSON
import           Control.Monad                  ( when )

----------------------------------------------------------------------------------------------------
-- | Update Topic
----------------------------------------------------------------------------------------------------

{-@ updateTopic :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
updateTopic :: RoomId -> Controller ()
updateTopic roomId = do
  viewer   <- requireAuthUser
  userRoom <- project userRoom' viewer
  vis      <- project userVisibility' viewer
  case userRoom == Just roomId of
    True | vis == "public" -> do
      UpdateTopicReq {..} <- decodeBody
      _                   <- validateTopic updateTopicReqTopic
      _ <- updateWhere (roomId' ==. roomId) (roomTopic' `assign` updateTopicReqTopic)
      room                <- selectFirstOr notFoundJSON (roomId' ==. roomId)
      roomData            <- extractRoomData room
      respondJSON status200 roomData
    True  -> respondError status409 (Just "This operation may leak information")
    False -> respondError status403 Nothing

newtype UpdateTopicReq = UpdateTopicReq { updateTopicReqTopic :: Text } deriving Generic

instance FromJSON UpdateTopicReq where
  parseJSON = genericParseJSON (stripPrefix "updateTopicReq")

{-@ validateTopic :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
validateTopic :: Text -> Controller ()
validateTopic topic = whenT (T.length topic > 50) $ respondError status400 (Just "Topic too long")

----------------------------------------------------------------------------------------------------
-- | Room Update
----------------------------------------------------------------------------------------------------

{-@ roomUpdate :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomUpdate :: RoomId -> Controller ()
roomUpdate roomId = do
  viewer            <- requireAuthUser
  _                 <- checkOrganizer viewer
  r@RoomInsert {..} <- decodeBody
  _                 <- validateRoom r
  let up =
        (roomColor' `assign` insertColor)
          `combine` (roomName' `assign` insertName)
          `combine` (roomTopic' `assign` insertTopic)
          `combine` (roomCapacity' `assign` insertCapacity)
          `combine` (roomZoomLink' `assign` insertZoomLink)
  _        <- updateWhere (roomId' ==. roomId) up
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  roomData <- extractRoomData room
  respondJSON status200 roomData

{-@ validateRoom :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ _ @-}
validateRoom :: RoomInsert -> Controller ()
validateRoom RoomInsert {..} = validateTopic insertTopic

----------------------------------------------------------------------------------------------------
-- | Room Batch Update
----------------------------------------------------------------------------------------------------

{-@ roomBatchUpdate :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomBatchUpdate :: Controller ()
roomBatchUpdate = do
  viewer                    <- requireAuthUser
  _                         <- checkOrganizer viewer
  (PostReq inserts updates) <- decodeBody

  let rooms = map
        (\RoomInsert {..} -> mkRoom insertColor insertName insertTopic insertCapacity insertZoomLink
        )
        inserts
  ids <- insertMany rooms
  _   <- forT updates $ \RoomData {..} -> do
    let up =
          (roomColor' `assign` roomColor)
            `combine` (roomName' `assign` roomName)
            `combine` (roomTopic' `assign` roomTopic)
            `combine` (roomCapacity' `assign` roomCapacity)
            `combine` (roomZoomLink' `assign` roomZoomLink)
    updateWhere (roomId' ==. roomId) up

  respondJSON status200 ids

data PostReq = PostReq
  { postReqInserts :: [RoomInsert]
  , postReqUpdates :: [RoomData]
  }
  deriving Generic

instance FromJSON PostReq where
  parseJSON = genericParseJSON (stripPrefix "postReq")

----------------------------------------------------------------------------------------------------
-- | Join Room
----------------------------------------------------------------------------------------------------

{-@ joinRoom :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
joinRoom :: RoomId -> Controller ()
joinRoom roomId = do
  viewer      <- requireAuthUser
  viewerId    <- project userId' viewer
  currentRoom <- project userRoom' viewer
  room        <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  zoomLink    <- project roomZoomLink' room

  whenT (currentRoom == Just roomId) (respondJSON status200 zoomLink)

  usersCount <- count (userRoom' ==. Just roomId)
  capacity   <- project roomCapacity' room
  if capacity <= 0 || usersCount < capacity
    then do
      _ <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Just roomId)
      respondJSON status200 zoomLink
    else respondError status409 (Just "Capacity Exceeded")

----------------------------------------------------------------------------------------------------
-- | Leave Room
----------------------------------------------------------------------------------------------------

{-@ leaveRoom :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
leaveRoom :: Controller ()
leaveRoom = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Nothing)
  respondTagged (emptyResponse status200)

----------------------------------------------------------------------------------------------------
-- | Room Get
----------------------------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  _     <- requireAuthUser
  rooms <- selectList trueF
  rooms <- mapT extractRoomData rooms
  respondJSON status200 rooms


{-@ extractRoomData :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
extractRoomData :: Entity Room -> Controller RoomData
extractRoomData room =
  RoomData
    `fmap` project roomId'       room
    <*>    project roomColor'    room
    <*>    project roomName'     room
    <*>    project roomTopic'    room
    <*>    project roomCapacity' room
    <*>    project roomZoomLink' room

-- | RoomInsert

data RoomInsert = RoomInsert
  { insertColor :: Text
  , insertName :: Text
  , insertTopic :: Text
  , insertCapacity :: Int
  , insertZoomLink :: Text
  }
  deriving Generic

instance FromJSON RoomInsert where
  parseJSON = genericParseJSON (stripPrefix "insert")

instance ToJSON RoomInsert where
  toEncoding = genericToEncoding (stripPrefix "insert")

-- | RoomUpdate

data RoomData = RoomData
  { roomId :: RoomId
  , roomColor :: Text
  , roomName :: Text
  , roomTopic :: Text
  , roomCapacity :: Int
  , roomZoomLink :: Text
  }
  deriving Generic

instance FromJSON RoomData where
  parseJSON = genericParseJSON (stripPrefix "room")

instance ToJSON RoomData where
  toEncoding = genericToEncoding (stripPrefix "room")
