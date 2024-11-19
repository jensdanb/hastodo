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
type TodoList = [Todo]

type TodoMap = Map UUID Todo

data Todo = Todo
    { todoId :: UUID
    , name :: Name
    , completed :: Active
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

--- 
--- State
--- 

type TodoVar = TVar TodoList
newtype State = State { todos :: TVar TodoList} deriving (Generic)

--- 
--- Logic
--- 

initialize :: IO TodoVar
initialize = newTVarIO initialList

insertTodo :: TodoVar -> Todo -> IO ()
insertTodo todoVar newTodo = atomically $ readTVar todoVar >>= writeTVar todoVar . (newTodo:)

--- 
--- Defaults and templates
--- 

initialList :: TodoList
initialList = []

mockUUID :: UUID
mockUUID = "sgsgerjkg"

mockUUID2 :: UUID
mockUUID2 = "ioreijojf"

mockUUID3 :: UUID
mockUUID3 = "eroigjowgjo"

mockTodo :: Todo
mockTodo = Todo {todoId="todo-1sgsgerjkg", name="Eat", completed=True}

mock2 = Todo {todoId="todo-2sigisgoe",name="Sleep", completed=False}

mock3 :: Todo
mock3 = Todo {todoId="todo-3efkif",name="Repeat", completed=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do 
  insertTodo todoVar mockTodo
  insertTodo todoVar mock2
  insertTodo todoVar mock3
