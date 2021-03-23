{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Message
  ( sendMessage
  , recvMessage
  , readMessage
  , messagesFor
  , RecvMessage
  )
where

import qualified Data.Text                     as T
import           Data.Int                       ( Int64 )
import           GHC.Generics

import           Storm.Core
import           Storm.Actions
import           Storm.Updates
import           Storm.Insert
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Storm.JSON

import           Controllers
import           Model
import           JSON

data SendMessage = SendMessage
  { sendMsgSenderId    :: UserId
  , sendMsgReceiverId  :: Maybe UserId
  , sendMsgMessageText :: T.Text
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
  , recvMsgMessageText :: T.Text
  , recvMsgTimestamp   :: Int64
  , recvMsgMessageId   :: MessageId
  }
  deriving Generic

instance FromJSON RecvMessage where
  parseJSON = genericParseJSON (stripPrefix "recvMsg")

instance ToJSON RecvMessage where
  toEncoding = genericToEncoding (stripPrefix "recvMsg")

-- | `sendMessage` accepts a new message from the current userId ---------------------

{-@ sendMessage :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
sendMessage :: Controller ()
sendMessage = do
  sender               <- requireAuthUser
  senderId             <- project userId' sender
  msg@SendMessage {..} <- decodeBody
  insert (mkMessage senderId sendMsgReceiverId sendMsgMessageText sendMsgTimestamp)
  respondJSON status200 ("OK:sendMessage" :: String)

-- | `readMessage msgId` marks `msgId` as the high-water mark for userId -------------

{-@ readMessage :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
readMessage :: MessageId -> Controller ()
readMessage msgId = do
  user   <- requireAuthUser
  userId <- project userId' user
  markMb <- selectFirst (markReadUser' ==. userId)
  case markMb of
    Nothing -> do
      insert (mkMarkRead userId msgId)
      respondJSON status200 ("OK:readMessage:insert" :: String)
    Just mark -> do
      upto <- project markReadUpto' mark
      updateWhere (markReadUser' ==. userId) (markReadUpto' `assign` (max upto msgId))
      respondJSON status200 ("OK:readMessage:update" :: String)

-- | `recvMessage` responds with all the (recent) messages for the current user ------

{-@ recvMessage :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
recvMessage :: Controller ()
recvMessage = do
  user   <- requireAuthUser
  userId <- project userId' user
  sends  <- messagesFor userId
  respondJSON status200 sends

{-@ messagesFor :: u: _ -> TaggedT<{\v -> entityKey v == u}, {\_ -> False}> _ _ _ @-}
messagesFor :: UserId -> Controller [RecvMessage]
messagesFor userId = do
  uptoMb <- selectFirst (markReadUser' ==. userId)
  lowerF <- case uptoMb of
    Nothing   -> return trueF
    Just upto -> do
      uptoId <- project markReadUpto' upto
      return (messageId' >. uptoId)
  msgs <- selectList
    (lowerF &&: ((messageReceiver' ==. Nothing) ||: (messageReceiver' ==. Just userId)))
  mapT extractRecvMessage msgs

{-@ extractRecvMessage :: m: _ -> TaggedT<{\v -> CanReadMessage m v}, {\_ -> False}> _ _ _ @-}
extractRecvMessage :: Entity Message -> Controller RecvMessage
extractRecvMessage msg =
  RecvMessage
    `fmap` project messageSender'    msg
    <*>    project messageReceiver'  msg
    <*>    project messageMessage'   msg
    <*>    project messageTimestamp' msg
    <*>    project messageId'        msg
