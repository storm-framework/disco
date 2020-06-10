{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Frankie                        ( HostPreference
                                                , Port
                                                )
import qualified Data.Text                     as T
import           Data.Maybe
import           GHC.Exts                       ( fromString )
import           System.Console.CmdArgs
import           System.Environment
import           Server

import           Auth                           ( addOrganizer
                                                , UserCreate(..)
                                                )

main :: IO ()
main = do
  args <- cmdArgs (modes [server &= auto, addorganizer &= explicit &= name "add-organizer"])
  case args of
    Server {..} -> do
      envStage <- readStageFromEnv
      runServer $ ServerOpts port (fromString host) (fromMaybe envStage stage) pool db
    AddOrganizer {..} -> do
      let user = UserCreate email password Nothing "" "" "" ""
      runTask' db $ addOrganizer user
      return ()

readStageFromEnv :: IO Stage
readStageFromEnv = do
  envStage <- lookupEnv "DISCO_STAGE"
  case envStage of
    Just "prod" -> return Prod
    _           -> return Dev

data Disco =
    Server { port :: Port , host :: String , stage :: Maybe Stage , pool :: Int, db :: T.Text}
  | AddOrganizer { email :: T.Text, password :: T.Text, db :: T.Text }
  deriving (Data, Typeable, Show)

server = Server
  { port  = 3000 &= typ "PORT" &= help "The port to bind to (default 3000)"
  , host  = "127.0.0.1" &= typ "HOST" &= help "The interface to bind to (default 127.0.0.1)"
  , stage = def &= typ "STAGE" &= help "Either dev or prod (default dev)"
  , pool  = 1 &= typ "SIZE" &= help "Sql Backend pool size (default 1)"
  , db    = "db.sqlite" &= typ "PATH" &= help "Database path (default db.sqlite)"
  }

addorganizer = AddOrganizer
  { email    = "" &= typ "EMAIL"
  , password = "" &= typ "PASSWORD"
  , db       = "db.sqlite" &= typ "PATH" &= help "Database path (default db.sqlite)"
  }
