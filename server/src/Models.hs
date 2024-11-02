{-# LANGUAGE DeriveGeneric #-}
module Models where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data ServerConnected = ServerConnected 
  { message :: String } deriving (Generic)

instance ToJSON ServerConnected