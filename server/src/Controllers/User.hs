{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

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
import           Controllers.Invitation         ( InvitationCode(..) )
import           Model
import           JSON

-------------------------------------------------------------------------------
-- | User Get
-------------------------------------------------------------------------------

{-@ userGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userGet :: Controller ()
userGet = do
  _     <- requireAuthUser
  users <- selectList trueF
  users <- mapMC extractUserData users
  respondJSON status200 users

extractUserData :: Entity User -> Controller UserData
extractUserData u = do
  id           <- project userId' u
  emailAddress <- project userEmailAddress' u
  photoURL     <- project userPhotoURL' u
  firstName    <- project userFirstName' u
  lastName     <- project userLastName' u
  displayName  <- project userDisplayName' u
  institution  <- project userInstitution' u
  country      <- project userCountry' u
  degree       <- project userDegree' u
  level        <- project userLevel' u
  visibility   <- project userVisibility' u
  room         <- if visibility == "public" then project userRoom' u else return Nothing
  return $ UserData id
                    emailAddress
                    photoURL
                    firstName
                    lastName
                    displayName
                    institution
                    country
                    degree
                    level
                    room

data UserData = UserData
  { userId :: UserId
  , userEmailAddress :: Text
  , userPhotoURL :: Maybe Text
  , userFirstName :: Text
  , userLastName :: Text
  , userDisplayName :: Text
  , userInstitution :: Text
  , userCountry :: Text
  , userDegree :: Text
  , userLevel :: String
  , userRoom :: Maybe RoomId
  }
  deriving Generic

instance ToJSON UserData where
  toEncoding = genericToEncoding (stripPrefix "user")
