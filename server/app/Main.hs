{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           System.Environment             ( getArgs )
import           Frankie                        ( HostPreference
                                                , Port
                                                )
import           System.Console.CmdArgs
import           Server
import           GHC.Exts                       ( fromString )

main :: IO ()
main = do
  Covideo {..} <- cmdArgs covideo
  runServer (fromString host) port

data Covideo = Covideo
  { port :: Port
  , host :: String
  }
  deriving (Data, Typeable, Show)

covideo =
  Covideo { port = 3000 &= typ "PORT" &= help "The port to bind to (default 3000)"
          , host = "127.0.0.1" &= typ "HOST" &= help "The interface to bind to (default 127.0.0.1)"
          }
    &= summary "CoVideo server"
