{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar)

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

initialize :: IO TodoVar
initialize = newTVarIO []

--- 
--- Logic
--- 

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> IO ()
modifyTodoList f tVar = atomically $ modifyTVar tVar f

insertTodo :: Todo -> TodoVar -> IO ()
insertTodo newTodo = modifyTodoList (newTodo:)

deleteTodo :: UUID -> TodoVar -> IO ()
deleteTodo uuid = modifyTodoList (filter ((/= uuid) . (.id)))


{-
changeTodoName :: TodoVar -> UUID -> Name -> IO ()
changeTodoName todoVar todoId name = atomically $ readTVar todoVar >>= modifyTVar todoVar . (newTodo:)
-}

rename :: Todo -> Name -> Todo
rename todo name = todo {name=name}

--- 
--- Defaults and templates
--- 

mock1 :: Todo
mock1 = Todo {id="todo-1sgsgerjkg", name="Eat", completed=True}

mock2 :: Todo
mock2 = Todo {id="todo-2sigisgoel", name="Sleep", completed=False}

mock3 :: Todo
mock3 = Todo {id="todo-3efkiffieu", name="Repeat", completed=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do
  insertTodo mock2 todoVar
  insertTodo mock3 todoVar
  insertTodo mock1 todoVar
