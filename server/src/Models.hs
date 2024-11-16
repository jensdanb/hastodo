{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map (Map)
import Control.Concurrent.STM (TVar)
import qualified Data.Map as Map 

newtype ServerConnected where
  ServerConnected :: {message :: String} -> ServerConnected
  deriving (Generic)

instance ToJSON ServerConnected

type UUID = Text
type Name = Text
type Active = Bool

type TodoMap = Map UUID Todo
type TodoVar = TVar TodoMap

data Todo = Todo
    { uuid :: UUID 
    , name :: Text
    , completed :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

initialState :: TodoMap
initialState = Map.empty

insertTodo :: TodoMap -> Todo -> TodoMap
insertTodo todoMap todo@(Todo uuid' name' completed') = 
    Map.insert uuid' todo todoMap 