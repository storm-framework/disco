{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSON
    ( defaultOptions
    , module A
    )
where

import           Data.Aeson              hiding ( defaultOptions )
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( FromJSON )
import           Data.Aeson.Types               ( Parser )
import           Data.Char
import           Data.List
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Type.Reflection                ( Typeable
                                                , typeRep
                                                )


defaultOptions :: A.Options
defaultOptions = A.defaultOptions { A.fieldLabelModifier     = headToLower . stripFieldPrefix
                                  , A.constructorTagModifier = headToLower . stripConstructorPrefix
                                  , A.sumEncoding            = A.ObjectWithSingleField
                                  }


{-@ ignore headToLower @-}
headToLower :: String -> String
headToLower []       = error "Can not use headToLower on empty String"
headToLower (x : xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

stripConstructorPrefix :: String -> String
stripConstructorPrefix t = maybe t (flip drop t . decrementSafe) $ findIndex isLower t
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1
