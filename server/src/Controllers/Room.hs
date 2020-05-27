{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}


module Controllers.Room where

import           Data.Text                      ( Text )
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

-------------------------------------------------------------------------------
-- | Room Post
-------------------------------------------------------------------------------

{-@ roomPost :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomPost :: Controller ()
roomPost = do
  viewer                    <- requireAuthUser
  _                         <- requireOrganizer viewer
  (PostReq inserts updates) <- decodeBody

  let rooms = map (\RoomData {..} -> mkRoom roomName roomCapacity roomZoomLink) inserts
  ids <- insertMany rooms
  _   <- forMC updates $ \(RoomEntity id RoomData {..}) -> do
    let up1 = roomName' `assign` roomName
    let up2 = roomCapacity' `assign` roomCapacity
    let up3 = roomZoomLink' `assign` roomZoomLink
    let up  = up1 `combine` up2 `combine` up3
    updateWhere (roomId' ==. id) up

  respondJSON status200 ids

data PostReq = PostReq
  { postReqInserts :: [RoomData]
  , postReqUpdates :: [RoomEntity]
  }
  deriving Generic

instance FromJSON PostReq where
  parseJSON = genericParseJSON (stripPrefix "postReq")

-------------------------------------------------------------------------------
-- | Join Room
-------------------------------------------------------------------------------

{-@ joinRoom :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
joinRoom :: Int64 -> Controller ()
joinRoom rid = do
  let roomId = toSqlKey rid
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  room     <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  _        <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Just roomId)
  zoomLink <- project roomZoomLink' room
  respondJSON status200 zoomLink

-------------------------------------------------------------------------------
-- | Room Get
-------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  _     <- requireAuthUser
  rooms <- selectList trueF
  rooms <- forMC rooms $ \room -> do
    id       <- project roomId' room
    roomData <-
      RoomData
      <$> project roomName'     room
      <*> project roomCapacity' room
      <*> project roomZoomLink' room
    return $ RoomEntity id roomData
  respondJSON status200 rooms

-- | RoomEntity

data RoomEntity = RoomEntity
  { roomEntityId :: RoomId
  , roomEntityData :: RoomData
  }
  deriving Generic

instance FromJSON RoomEntity where
  parseJSON = genericParseJSON (stripPrefix "roomEntity")

instance ToJSON RoomEntity where
  toEncoding = genericToEncoding (stripPrefix "roomEntity")
-- | RoomData

data RoomData = RoomData
  { roomName :: Text
  , roomCapacity :: Int
  , roomZoomLink :: Text
  }
  deriving Generic

instance FromJSON RoomData where
  parseJSON = genericParseJSON (stripPrefix "room")

instance ToJSON RoomData where
  toEncoding = genericToEncoding (stripPrefix "room")
