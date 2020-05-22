{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}


module Controllers.Room where

import           Data.Text                      ( Text )
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

  ids <- insertMany (map (\RoomData {..} -> mkRoom name capacity zoomLink) inserts)
  _                         <- flip mapMC updates $ \(RoomEntity id RoomData {..}) -> do
    let up1 = roomName' `assign` name
    let up2 = roomCapacity' `assign` capacity
    let up3 = roomZoomLink' `assign` zoomLink
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
-- | Room Get
-------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  rooms <- selectList trueF
  rooms <- flip mapMC rooms $ \room -> do
    id       <- project roomId' room
    roomData <-
      RoomData
      <$> project roomName'     room
      <*> project roomCapacity' room
      <*> project roomZoomLink' room
    return $ RoomEntity id roomData
  respondJSON status200 rooms

data RoomData = RoomData
  { name :: Text
  , capacity :: Int
  , zoomLink :: Text
  }
  deriving Generic

instance FromJSON RoomData where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RoomData where
  toEncoding = genericToEncoding defaultOptions

data RoomEntity = RoomEntity
  { roomEntityId :: RoomId
  , roomEntityRoom :: RoomData
  }
  deriving Generic

instance FromJSON RoomEntity where
  parseJSON = genericParseJSON (stripPrefix "roomEntity")

instance ToJSON RoomEntity where
  toEncoding = genericToEncoding (stripPrefix "roomEntity")
