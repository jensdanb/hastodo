{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (liftIO)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)

--- 
--- Definitions
---

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

--- 
--- State
--- 

type TodoVar = TVar TodoMap
newtype State = State { todos :: TVar TodoMap} deriving (Generic)

--- 
--- Logic
--- 

initialize :: IO TodoVar
initialize = newTVarIO initialMap

insertTodo :: TodoVar -> TodoMap -> IO ()
insertTodo todoVar newTodo = atomically $ readTVar todoVar >>= writeTVar todoVar . Map.union newTodo

--- 
--- Defaults and templates
--- 

initialMap :: TodoMap
initialMap = Map.empty

mockUUID :: UUID
mockUUID = "sgsgerjkg"

mockUUID2 :: UUID
mockUUID2 = "ioreijojf"

mockUUID3 :: UUID
mockUUID3 = "eroigjowgjo"

mockTodo :: Todo
mockTodo = Todo {name="Eat", completed=True}

mock2 = Todo {name="Sleep", completed=False}

mock3 :: Todo
mock3 = Todo {name="Repeat", completed=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do 
  insertTodo todoVar (Map.fromList [(mockUUID, mockTodo), (mockUUID2, mock2), (mockUUID3, mock3)])
