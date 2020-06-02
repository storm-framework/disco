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
-- | User Put
-------------------------------------------------------------------------------


{-@ userPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userPut :: Controller ()
userPut = do
  (PutReq (InvitationCode id code) UserCreate {..}) <- decodeBody
  let user = mkUser emailAddress
                    password
                    firstName
                    lastName
                    displayName
                    institution
                    country
                    degree
                    "attendee"
                    "public"
                    Nothing
  _ <- selectFirstOr
    (errorResponse status403 (Just "invalid invitation"))
    (   (invitationId' ==. id)
    &&: (invitationCode' ==. code)
    &&: (invitationEmailAddress' ==. emailAddress)
    &&: (invitationAccepted' ==. False)
    )
  userId <- insert user
  _      <- updateWhere (invitationId' ==. id) (invitationAccepted' `assign` True)
  respondJSON status201 (object ["id" .= userId])

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
                    firstName
                    lastName
                    displayName
                    institution
                    country
                    degree
                    level
                    room

data PutReq = PutReq
  { putReqInvitationCode :: InvitationCode
  , putReqUser :: UserCreate
  }
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON (stripPrefix "putReq")

data UserCreate = UserCreate
  { emailAddress :: Text
  , password :: Text
  , firstName :: Text
  , lastName :: Text
  , displayName :: Text
  , institution :: Text
  , country :: Text
  , degree :: Text
  }
  deriving Generic

instance FromJSON UserCreate where
  parseJSON = genericParseJSON defaultOptions

data UserData = UserData
  { userId :: UserId
  , userEmailAddress :: Text
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
