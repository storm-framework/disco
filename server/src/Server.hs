{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Server
    ( runServer
    , runTask'
    , initDB
    , ServerOpts(..)
    , Stage(..)
    )
where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend
                                                , runSqlite
                                                , runMigration
                                                , createSqlitePool
                                                )
import           System.FilePath               as P
import           System.Directory
import           System.Environment
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString               as BS
import           Network.Mime
import           Frankie.Config
import           Frankie.Auth
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import qualified Data.Pool                     as Pool
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger           ( runNoLoggingT )
import qualified Control.Concurrent.MVar       as MVar
import           Control.Lens.Operators         ( (^.) )
import qualified Text.Mustache.Types           as Mustache
import           Text.Read                      ( readMaybe )
import           Data.Typeable
import           Data.Data                      ( Data )

import           Binah.Core
import           Binah.Frankie
import           Binah.Infrastructure
import           Binah.Insert
import           Binah.Actions
import           Binah.Filters
import           Binah.JSON

import           Binah.SMTP             -- TODO: DUMMY RECURSIVE IMPORTS for LH
import           Binah.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 


import           Controllers
import           Controllers.Invitation
import           Controllers.User
import           Controllers.Room
import           Controllers.Message
import           Controllers.Sync
import           Model
import           Auth

import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           Network.Socket                 ( PortNumber )

data Stage = Prod | Dev deriving (Data, Typeable, Show)

data ServerOpts = ServerOpts
  { optsPort :: Port
  , optsHost :: HostPreference
  , optsStatic :: Maybe String
  , optsPool :: Int
  , optsDBPath :: T.Text
  }

{-@ ignore runServer @-}
runServer :: ServerOpts -> IO ()
runServer ServerOpts {..} = runNoLoggingT $ do
    liftIO $ initDB optsDBPath
    cfg  <- liftIO readConfig
    pool <- createSqlitePool optsDBPath optsPool
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host optsHost
            port optsPort
            initWithT $ initFromPool cfg pool
        dispatch $ do
            post "/api/signin"             signIn
            post "/api/signup"             signUp
            post "/api/signout"            signOut
            put  "/api/invitation"         invitationPut
            get  "/api/invitation/:id"     invitationGet
            get  "/api/invitation"         invitationList
            get  "/api/user"               userList
            get  "/api/user/:id"           userGet
            post "/api/user/me"            userUpdateMe
            get  "/api/room"               roomGet
            post "/api/room"               roomBatchUpdate
            post "/api/room/current/leave" leaveRoom
            post "/api/room/:id/update"    roomUpdate
            post "/api/room/:id/topic"     updateTopic
            post "/api/room/:id/join"      joinRoom
            post "/api/room/joinRandom"    joinRandom
            get  "/api/signurl"            presignS3URL
            post "/api/message/send"       sendMessage
            post "/api/message/read/:id"   readMessage
            get  "/api/message/receive"    recvMessage
            post "/api/beacon"             beacon
            post "/api/sync"               sync
            put  "/api/photo/:id"          photoPut
            get  "/api/photo/:id"          photoGet

            case optsStatic of
                Just path -> fallback (sendFromDirectory path "index.html")
                Nothing   -> fallback $ do
                    req <- request
                    let path = joinPath (map T.unpack (reqPathInfo req))
                    respondJSON status404 ("Route not found: " ++ path)


runTask' :: T.Text -> Task a -> IO a
runTask' dbpath task = runSqlite dbpath $ do
    cfg     <- liftIO readConfig
    backend <- ask
    liftIO . runTIO $ configure cfg (runReaderT (unTag task) backend)

readConfig :: IO Config
readConfig =
    Config authMethod <$> MVar.newMVar mempty <*> readAWSConfig <*> readSMTPConfig <*> readSecretKey


readSecretKey :: IO BS.ByteString
readSecretKey = do
    secret <- fromMaybe "sb8NHmF@_-nsf*ymt!wJ3.KXmTDPsNoy" <$> lookupEnv "DISCO_SECRET_KEY"
    return $ T.encodeUtf8 . T.pack $ secret


readAWSConfig :: IO AWSConfig
readAWSConfig = do
    accessKey <- fromMaybe ""                  <$> lookupEnv "DISCO_AWS_ACCESS_KEY"
    secretKey <- fromMaybe ""                  <$> lookupEnv "DISCO_AWS_SECRET_KEY"
    region    <- readMaybe . fromMaybe ""      <$> lookupEnv "DISCO_AWS_REGION"
    bucket    <- fromMaybe "distant-socialing" <$> lookupEnv "DISCO_AWS_BUCKET"
    env       <- AWS.newEnv $ AWS.FromKeys (AWS.AccessKey $ T.encodeUtf8 $ T.pack accessKey)
                                           (AWS.SecretKey $ T.encodeUtf8 $ T.pack secretKey)
    return $ AWSConfig { awsAuth   = env ^. AWS.envAuth
                       , awsRegion = fromMaybe AWS.NorthCalifornia region
                       , awsBucket = S3.BucketName (T.pack bucket)
                       }

readSMTPConfig :: IO SMTPConfig
readSMTPConfig = do
    host <- fromMaybe "localhost" <$> lookupEnv "DISCO_SMTP_HOST"
    port <- fromMaybe "425"       <$> lookupEnv "DISCO_SMTP_PORT"
    user <- fromMaybe ""          <$> lookupEnv "DISCO_SMTP_USER"
    pass <- fromMaybe ""          <$> lookupEnv "DISCO_SMTP_PASS"
    return $ SMTPConfig host (read port :: PortNumber) user pass

{-@ ignore initDB @-}
initDB :: T.Text -> IO ()
initDB dbpath = runSqlite dbpath $ do
    runMigration migrateAll

-- Static files

{-@ ignore sendFromDirectory @-}
sendFromDirectory :: FilePath -> FilePath -> Controller ()
sendFromDirectory dir fallback = do
    req <- request
    let path = dir </> joinPath (map T.unpack (reqPathInfo req))
    exists <- liftTIO . TIO $ doesFileExist path
    if exists then sendFile path else sendFile (dir </> fallback)

{-@ ignore sendFile @-}
sendFile :: FilePath -> Controller ()
sendFile path = do
    let mime = defaultMimeLookup (T.pack path)
    content <- liftTIO . TIO . LBS.readFile $ path
    respondTagged $ Response status200 [(hContentType, mime)] content

-- TODO find a way to provide this without exposing the instance of MonadBaseControl

initFromPool
  :: Config
  -> Pool.Pool SqlBackend
  -> Controller ()
  -> TaggedT (Entity User) (ControllerT TIO) ()
initFromPool cfg pool = mapTaggedT run
    where run act = Pool.withResource pool $ configure cfg . runReaderT act

instance MonadBase IO TIO where
    liftBase = TIO

instance MonadBaseControl IO TIO where
    type StM TIO a = a
    liftBaseWith f = TIO (f runTIO)
    restoreM = return

instance MonadBase IO (ControllerT TIO) where
    liftBase = lift . liftBase

instance MonadBaseControl IO (ControllerT TIO) where
    type StM (ControllerT TIO) a = ControllerStatus a
    liftBaseWith f = ControllerT $ \r -> TIO $ fmap Working (f (runTIO . flip runController r))
    restoreM st = ControllerT $ \_ -> return st
