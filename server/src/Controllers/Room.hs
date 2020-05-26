{-# LANGUAGE TupleSections #-}
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
      <*> return Nothing
    return $ RoomEntity id roomData
  rooms <- if withUsers == ["true"] then fetchUsers rooms else return rooms
  respondJSON status200 rooms

{-@ fetchUsers :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
fetchUsers :: [RoomEntity] -> Controller [RoomEntity]
fetchUsers rooms = do
  users     <- selectList (userVisibility' ==. "public")
  usersRoom <- projectList userRoom' users
  let usersByRoom = mapMaybe (\(x, y) -> fmap (, y) x) (zip usersRoom users)
  forMC rooms $ \(RoomEntity id d) -> do
    users <- usersForRoom usersByRoom id
    return $ RoomEntity id (d { roomUsers = Just users })

{-@ usersForRoom :: _ -> _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
usersForRoom :: [(RoomId, Entity User)] -> RoomId -> Controller [UserData]
usersForRoom usersByRoom roomId = do
  flip mapMC (lookupAll roomId usersByRoom) $ \u -> do
    userId <- project userId' u
    name   <- project userDisplayName' u
    return $ UserData userId name

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll a ((a', b) : xs) | a == a'   = b : lookupAll a xs
                           | otherwise = lookupAll a xs

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
  , roomUsers :: Maybe [UserData]
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
