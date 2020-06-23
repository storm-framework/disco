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

-------------------------------------------------------------------------------
-- | Room Update
-------------------------------------------------------------------------------

{-@ roomUpdate :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomUpdate :: RoomId -> Controller ()
roomUpdate roomId = do
  r@RoomInsert {..} <- decodeBody
  validateRoom r
  let up =
        (roomColor' `assign` insertColor)
          `combine` (roomName' `assign` insertName)
          `combine` (roomTopic' `assign` insertTopic)
          `combine` (roomZoomLink' `assign` insertZoomLink)
  _        <- updateWhere (roomId' ==. roomId) up
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  roomData <- extractRoomData room
  respondJSON status200 roomData

validateRoom :: RoomInsert -> Controller ()
validateRoom RoomInsert {..} = do
  when (T.length insertTopic > 50) $ respondError status400 (Just "Topic too long")


-------------------------------------------------------------------------------
-- | Room Batch Update
-------------------------------------------------------------------------------

{-@ roomBatchUpdate :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomBatchUpdate :: Controller ()
roomBatchUpdate = do
  viewer                    <- requireAuthUser
  _                         <- requireOrganizer viewer
  (PostReq inserts updates) <- decodeBody

  let rooms =
        map (\RoomInsert {..} -> mkRoom insertColor insertName insertTopic insertZoomLink) inserts
  ids <- insertMany rooms
  _   <- forT updates $ \RoomData {..} -> do
    let up1 = roomColor' `assign` roomColor
    let up2 = roomName' `assign` roomName
    let up3 = roomTopic' `assign` roomTopic
    let up4 = roomZoomLink' `assign` roomZoomLink
    let up  = up1 `combine` up2 `combine` up3 `combine` up4
    updateWhere (roomId' ==. roomId) up

  respondJSON status200 ids

data PostReq = PostReq
  { postReqInserts :: [RoomInsert]
  , postReqUpdates :: [RoomData]
  }
  deriving Generic

instance FromJSON PostReq where
  parseJSON = genericParseJSON (stripPrefix "postReq")


-------------------------------------------------------------------------------
-- | Join Room
-------------------------------------------------------------------------------

{-@ joinRoom :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
joinRoom :: RoomId -> Controller ()
joinRoom roomId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Just roomId)
  zoomLink <- project roomZoomLink' room
  respondJSON status200 zoomLink

-------------------------------------------------------------------------------
-- | Room Get
-------------------------------------------------------------------------------

{-@ leaveRoom :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
leaveRoom :: Controller ()
leaveRoom = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Nothing)
  respondJSON status200 (object [])

-------------------------------------------------------------------------------
-- | Room Get
-------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  _     <- requireAuthUser
  rooms <- selectList trueF
  rooms <- mapT extractRoomData rooms
  respondJSON status200 rooms


extractRoomData :: Entity Room -> Controller RoomData
extractRoomData room =
  RoomData
    <$> project roomId'       room
    <*> project roomColor'    room
    <*> project roomName'     room
    <*> project roomTopic'    room
    <*> project roomZoomLink' room

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
