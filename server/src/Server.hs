{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}

module Server
    ( runServer
    , runWorker'
    , initDB
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
import           Network.Mime
import           Frankie.Config
import           Frankie.Auth
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Data.Pool                      ( Pool )
import qualified Data.Pool                     as Pool
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger           ( runNoLoggingT )
import qualified Control.Concurrent.MVar       as MVar
import           Control.Lens.Lens              ( (&) )
import           Control.Lens.Operators         ( (^.) )
import qualified Text.Mustache.Types           as Mustache


import           Binah.Core
import           Binah.Frankie
import           Binah.Infrastructure
import           Binah.Insert
import           Binah.Actions
import           Binah.Filters

import           Controllers
import           Controllers.Invitation
import           Controllers.User
import           Controllers.Room
import           Model
import           Auth

import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3


{-@ ignore runServer @-}
runServer :: HostPreference -> Port -> IO ()
runServer h p = runNoLoggingT $ do
    templateCache <- liftIO $ MVar.newMVar mempty
    pool          <- createSqlitePool "db.sqlite" 1
    aws           <- liftIO getAwsConfig
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host h
            port p
            initWith $ initFromPool (Config authMethod templateCache aws) pool
        dispatch $ do
            post "/api/signin" signIn
            post "/api/signup" signUp
            put "/api/invitation" invitationPut
            get "/api/invitation/:id" invitationGet
            get "/api/invitation"     invitationList
            get "/api/user"           userList
            get "/api/user/:id"       userGet
            post "/api/user/me" userUpdateMe
            get "/api/room" roomGet
            post "/api/room"          roomBatchUpdate
            post "/api/room/:id"      roomUpdate
            post "/api/room/leave"    leaveRoom
            post "/api/room/:id/join" joinRoom
            get "/api/signurl" s3SignedURL

#ifdef PROD
            fallback (sendFromDirectory "static" "index.html")
#else
            fallback $ do
                req <- request
                let path = joinPath (map T.unpack (reqPathInfo req))
                respondJSON status404 ("Route not found: " ++ path)
#endif


runWorker' :: Worker a -> IO a
runWorker' worker = runSqlite "db.sqlite" $ do
    templateCache <- liftIO $ MVar.newMVar mempty
    aws           <- liftIO getAwsConfig
    backend       <- ask
    let config = Config authMethod templateCache aws backend
    liftIO . runTIO $ runReaderT (unConfigT (runReaderT (unTag worker) backend)) config


getAwsConfig :: IO AWSConfig
getAwsConfig = do
    accessKey <- getEnv "SOCIAL_DISTANCING_AWS_ACCESS_KEY"
    secretKey <- getEnv "SOCIAL_DISTANCING_AWS_SECRET_KEY"
    env       <- AWS.newEnv $ AWS.FromKeys (AWS.AccessKey $ T.encodeUtf8 $ T.pack accessKey)
                                           (AWS.SecretKey $ T.encodeUtf8 $ T.pack secretKey)
    return $ AWSConfig { awsAuth   = env ^. AWS.envAuth
                       , awsRegion = AWS.NorthCalifornia
                       , awsBucket = S3.BucketName "binah-social-distancing"
                       }

{-@ ignore initDB @-}
initDB :: IO ()
initDB = runSqlite "db.sqlite" $ do
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

initFromPool :: (SqlBackend -> Config) -> Pool SqlBackend -> Controller () -> ControllerT TIO ()
initFromPool mkConfig pool controller = Pool.withResource pool
    $ \sqlBackend -> configure (mkConfig sqlBackend) . reading backend . unTag $ controller

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
