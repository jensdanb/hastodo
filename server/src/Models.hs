{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar, newTVarIO)

newtype ServerConnected where
  ServerConnected :: {message :: String} -> ServerConnected
  deriving (Generic)

instance ToJSON ServerConnected

type UUID = Text
type Name = Text
type Active = Bool
type TodoKeyValue = (UUID, Todo)

type TodoMap = Map UUID Todo

data Todo = Todo
    { name :: Name
    , completed :: Active
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

--- Logic --- 

--- Defaults and templates ---

initialMap :: TodoMap
initialMap = Map.empty

mockUUID :: UUID
mockUUID = "sgsgerjkg"

mockTodo :: Todo
mockTodo = Todo {name="Eat", completed=False}

--- State --- 

type TodoVar = TVar TodoMap
newtype State = State { todos :: TVar TodoMap} deriving (Generic)

initialize :: IO TodoVar
initialize = newTVarIO initialMap