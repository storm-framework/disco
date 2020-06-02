{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Server
    ( runServer
    , initDB
    )
where

import qualified Control.Concurrent.MVar       as MVar
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
import qualified Data.ByteString.Lazy          as LBS
import           Network.Mime
import           Frankie.Config
import           Frankie.Auth
import qualified Data.Text                     as T

import           Data.Pool                      ( Pool )
import qualified Data.Pool                     as Pool
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger           ( runNoLoggingT )

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



{-@ ignore runServer @-}
runServer :: IO ()
runServer = runNoLoggingT $ do
    pool <- createSqlitePool "db.sqlite" 1
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host "127.0.0.1" -- Note: this is not the same as "localhost", which might use IPv6
            port 3000
            initWith $ initFromPool pool
        dispatch $ do
            post "/api/signin" signIn
            put "/api/invitation" invitationPut
            get "/api/invitation/:id" invitationGet
            get "/api/invitation"     invitationIndex
            put "/api/user" userPut
            get "/api/user" userGet
            get "/api/room" roomGet
            post "/api/room"          roomPost
            post "/api/room/:id/join" joinRoom

            fallback (sendFromDirectory "static" "index.html")

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
    if exists
        then sendFile path
        else sendFile (dir </> fallback)

{-@ ignore sendFile @-}
sendFile :: FilePath -> Controller ()
sendFile path =  do
    let mime = defaultMimeLookup (T.pack path)
    content <- liftTIO . TIO . LBS.readFile $ path
    respondTagged $ Response status200 [(hContentType, mime)] content

-- TODO find a way to provide this without exposing the instance of MonadBaseControl

initFromPool :: Pool SqlBackend -> Controller () -> ControllerT TIO ()
initFromPool pool controller = Pool.withResource pool $ \sqlBackend ->
    configure (Config sqlBackend authMethod) . reading backend . unTag $ controller

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
