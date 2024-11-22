{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)

--- 
--- Definitions
---
type TodoKeyValue = (UUID, Todo)
type TodoList = [Todo]

type UUID = Text
type Name = Text

data Todo = Todo
    { id :: UUID
    , name :: Name
    , completed :: Bool
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

{-
changeTodoName :: TodoVar -> UUID -> Name -> IO ()
changeTodoName todoVar todoId name = atomically $ readTVar todoVar >>= modifyTVar todoVar . (newTodo:)
-}

rename :: Todo -> Name -> Todo 
rename todo name = todo {name=name}

--- 
--- Defaults and templates
--- 

initialList :: TodoList
initialList = []

mock1 :: Todo
mock1 = Todo {id="todo-1sgsgerjkg", name="Eat", completed=True}

mock2 :: Todo
mock2 = Todo {id="todo-2sigisgoel", name="Sleep", completed=False}

mock3 :: Todo
mock3 = Todo {id="todo-3efkiffieu", name="Repeat", completed=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do 
  insertTodo todoVar mock1
  insertTodo todoVar mock2
  insertTodo todoVar mock3
