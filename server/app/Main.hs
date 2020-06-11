{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Frankie                        ( HostPreference
                                                , Port
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Data.Maybe
import           GHC.Exts                       ( fromString )
import           System.Console.CmdArgs
import           System.Environment
import           Server

import           Auth                           ( addOrganizer
                                                , UserCreate(..)
                                                )
import           Controllers.Invitation

main :: IO ()
main = do
  args <- cmdArgs (modes allModes)
  case args of
    Server {..} -> do
      runServer $ ServerOpts port (fromString host) static pool db
    AddOrganizer {..} -> do
      let user = UserCreate email password Nothing "" "" "" "" ""
      runTask' db $ addOrganizer user
      return ()
    SendInvitation {..} -> runTask' db $ sendEmail invitationId


data Disco
  = Server { port :: Port
           , host :: String
           , static :: Maybe String
           , pool :: Int
           , db :: T.Text
           }
  | AddOrganizer { email :: T.Text
                 , password :: T.Text
                 , db :: T.Text
                 }
  | SendInvitation { invitationId :: Int64
                   , db :: T.Text
                   }
  deriving (Data, Typeable, Show)

allModes =
  [ server &= auto
  , addorganizer &= explicit &= name "add-organizer"
  , sendinvitation &= explicit &= name "send-invitation"
  ]

server = Server
  { port   = 3000 &= typ "PORT" &= help "The port to bind to (default 3000)"
  , host   = "127.0.0.1" &= typ "HOST" &= help "The interface to bind to (default 127.0.0.1)"
  , pool   = 1 &= typ "SIZE" &= help "Sql Backend pool size (default 1)"
  , static = def &= typ "PATH" &= help "If specified serve any unknown route from this directory"
  , db     = "db.sqlite" &= typ "PATH" &= help "Database path (default db.sqlite)"
  }

addorganizer = AddOrganizer
  { email    = "" &= typ "EMAIL"
  , password = "" &= typ "PASSWORD"
  , db       = "db.sqlite" &= typ "PATH" &= help "Database path (default db.sqlite)"
  }

sendinvitation = SendInvitation
  { invitationId = 0 &= typ "ID"
  , db = "db.sqlite" &= typ "PATH" &= help "Databse path (default db.sqlite"
  }
