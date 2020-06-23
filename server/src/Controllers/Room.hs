{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}


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

{-@ updateTopic :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
updateTopic :: RoomId -> Controller ()
updateTopic roomId = do
  viewer   <- requireAuthUser
  topic    <- decodeBody
  _        <- validateTopic topic
  _        <- updateWhere (roomId' ==. roomId) (roomTopic' `assign` topic)
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  roomData <- extractRoomData room
  respondJSON status200 roomData

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

  let rooms =
        map (\RoomInsert {..} -> mkRoom insertColor insertName insertTopic insertZoomLink) inserts
  ids <- insertMany rooms
  _   <- forMC updates $ \RoomData {..} -> do
    let up =
          (roomColor' `assign` roomColor)
            `combine` (roomName' `assign` roomName)
            `combine` (roomTopic' `assign` roomTopic)
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
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Just roomId)
  zoomLink <- project roomZoomLink' room
  respondJSON status200 zoomLink

----------------------------------------------------------------------------------------------------
-- | Leave Room
----------------------------------------------------------------------------------------------------

{-@ leaveRoom :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
leaveRoom :: Controller ()
leaveRoom = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Nothing)
  respondJSON status200 (object [])

----------------------------------------------------------------------------------------------------
-- | Room Get
----------------------------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  _     <- requireAuthUser
  rooms <- selectList trueF
  rooms <- mapMC extractRoomData rooms
  respondJSON status200 rooms


{-@ extractRoomData :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
extractRoomData :: Entity Room -> Controller RoomData
extractRoomData room = do
  id       <- project roomId' room
  color    <- project roomColor' room
  name     <- project roomName' room
  topic    <- project roomTopic' room
  zoomLink <- project roomZoomLink' room
  return $ RoomData id color name topic zoomLink

-- | RoomInsert

data RoomInsert = RoomInsert
  { insertColor :: Text
  , insertName :: Text
  , insertTopic :: Text
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
  , roomZoomLink :: Text
  }
  deriving Generic

instance FromJSON RoomData where
  parseJSON = genericParseJSON (stripPrefix "room")

instance ToJSON RoomData where
  toEncoding = genericToEncoding (stripPrefix "room")
