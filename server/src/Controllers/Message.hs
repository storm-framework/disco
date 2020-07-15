{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Message
  ( sendMessage
  , recvMessage
  , readMessage
  )
where

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
import           Model
import           JSON
import           Control.Monad                  ( when )

data SendMessage = SendMessage
  { sendMsgSenderId    :: UserId
  , sendMsgReceiverId  :: Maybe UserId
  , sendMsgMessageText :: Text
  , sendMsgTimestamp   :: Int64
  }
  deriving Generic

instance FromJSON SendMessage where
  parseJSON = genericParseJSON (stripPrefix "sendMsg")

instance ToJSON SendMessage where
  toEncoding = genericToEncoding (stripPrefix "sendMsg")

data RecvMessage = RecvMessage
  { recvMsgSenderId    :: UserId
  , recvMsgReceiverId  :: Maybe UserId
  , recvMsgMessageText :: Text
  , recvMsgTimestamp   :: Int64
  , recvMsgMessageId   :: MessageId
  }
  deriving Generic

instance FromJSON RecvMessage where
  parseJSON = genericParseJSON (stripPrefix "recvMsg")

instance ToJSON RecvMessage where
  toEncoding = genericToEncoding (stripPrefix "recvMsg")

-- | `sendMessage` accepts a new message from the current userId ---------------------

sendMessage :: Controller ()
sendMessage = do
  sender               <- requireAuthUser
  senderId             <- project userId' sender
  msg@SendMessage {..} <- decodeBody
  insert (mkMessage senderId sendMsgReceiverId sendMsgMessageText sendMsgTimestamp)
  respondJSON status200 ("OK:sendMessage" :: Text)

-- | `readMessage msgId` marks `msgId` as the high-water mark for userId -------------

readMessage :: MessageId -> Controller ()
readMessage msgId = do
  user   <- requireAuthUser
  userId <- project userId' user
  markMb <- selectFirst (markReadUser' ==. userId)
  case markMb of
    Nothing -> do
      insert (mkMarkRead userId msgId)
      respondJSON status200 ("OK:readMessage:insert" :: Text)
    Just mark -> do
      upto <- project markReadUpto' mark
      updateWhere (markReadUser' ==. userId) (markReadUpto' `assign` (max upto msgId))
      respondJSON status200 ("OK:readMessage:update" :: Text)

-- | `recvMessage` responds with all the (recent) messages for the current user ------

recvMessage :: Controller ()
recvMessage = do
  user   <- requireAuthUser
  userId <- project userId' user
  uptoMb <- selectFirst (markReadUser' ==. userId)
  lowerF <- case uptoMb of
    Nothing   -> return trueF
    Just upto -> do
      uptoId <- project markReadUpto' upto
      return (messageId' >. uptoId)
  msgs <- selectList
    (   lowerF
    &&: (messageReceiver' ==. Nothing ||: (messageReceiver' ==. Just userId))
    &&: (messageSender' !=. userId)
    )
  sends <- mapT extractRecvMessage msgs
  respondJSON status200 sends

extractRecvMessage :: Entity Message -> Controller RecvMessage
extractRecvMessage msg =
  RecvMessage
    `fmap` project messageSender'    msg
    <*>    project messageReceiver'  msg
    <*>    project messageMessage'   msg
    <*>    project messageTimestamp' msg
    <*>    project messageId'        msg
