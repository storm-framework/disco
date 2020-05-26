{-# LANGUAGE TupleSections #-}
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
  viewer       <- requireAuthUser
  viewerId     <- project userId' viewer
  room         <- selectFirstOr notFoundJSON (roomId' ==. roomId)
  _            <- updateWhere (userId' ==. viewerId) (userRoom' `assign` Just roomId)
  users        <- selectList (userVisibility' ==. "public" &&: userRoom' ==. Just roomId)
  users        <- extractUsersData users
  roomName     <- project roomName' room
  roomCapacity <- project roomCapacity' room
  roomZoomLink <- project roomZoomLink' room
  respondJSON status200 (RoomData roomName roomCapacity roomZoomLink users)

-------------------------------------------------------------------------------
-- | Room Get
-------------------------------------------------------------------------------

{-@ roomGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
roomGet :: Controller ()
roomGet = do
  withUsers <- queryParams "withUsers" :: Controller [String]
  rooms     <- selectList trueF
  rooms     <- forMC rooms $ \room -> do
    id       <- project roomId' room
    roomData <-
      RoomData
      <$> project roomName'     room
      <*> project roomCapacity' room
      <*> project roomZoomLink' room
      <*> return []
    return $ RoomEntity id roomData
  rooms <- if withUsers == ["true"] then appendUsers rooms else return rooms
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

{-@ appendUsers :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
appendUsers :: [RoomEntity] -> Controller [RoomEntity]
appendUsers rooms = do
  users     <- selectList (userVisibility' ==. "public")
  usersRoom <- projectList userRoom' users
  let usersByRoom = mapMaybe (\(x, y) -> fmap (, y) x) (zip usersRoom users)
  forMC rooms $ \(RoomEntity roomId roomData) -> do
    users <- extractUsersData (lookupAll roomId usersByRoom)
    return $ RoomEntity roomId (roomData { roomUsers = users })

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll a ((a', b) : xs) | a == a'   = b : lookupAll a xs
                           | otherwise = lookupAll a xs

-- | RoomData

data RoomData = RoomData
  { roomName :: Text
  , roomCapacity :: Int
  , roomZoomLink :: Text
  , roomUsers :: [UserData]
  }
  deriving Generic

instance FromJSON RoomData where
  parseJSON = genericParseJSON (stripPrefix "room")

instance ToJSON RoomData where
  toEncoding = genericToEncoding (stripPrefix "room")

-- | UserData

data UserData = UserData
  { userId          :: UserId
  , userDisplayName :: Text
  }
  deriving Generic

instance FromJSON UserData where
  parseJSON = genericParseJSON (stripPrefix "user")

instance ToJSON UserData where
  toEncoding = genericToEncoding (stripPrefix "user")

{-@ extractUsersData :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
extractUsersData :: [Entity User] -> Controller [UserData]
extractUsersData users = do
  forMC users $ \u -> do
    userId <- project userId' u
    name   <- project userDisplayName' u
    return $ UserData userId name
