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
type PutData = (UUID, Bool, Name)

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

putTodo :: PutData -> TodoVar -> IO ()
putTodo (uuid, toggle, newName) = modifyTodoList (map putter)
  where 
    putter :: Todo -> Todo 
    putter oldTodo = 
      if oldTodo.id /= uuid then oldTodo 
      else oldTodo { completed = if toggle then not oldTodo.completed else oldTodo.completed
                   , name = if newName=="" then oldTodo.name else newName}

replaceTodo :: Todo -> TodoVar -> IO ()
replaceTodo newTodo = modifyTodoList (map replaceIfSameId)
  where
    replaceIfSameId oldTodo = if oldTodo.id == newTodo.id then newTodo else oldTodo

{- Orphaned code
matchingId :: UUID -> Todo -> Bool
matchingId uuid todo = uuid == todo.id

findById :: UUID -> TodoList -> Maybe Todo
findById uuid = find (matchingId uuid)

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

mock4 :: Todo
mock4 = Todo {id="todo-efwpekkgwm", name="Repeat", completed=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do
  insertTodo mock1 todoVar
  insertTodo mock2 todoVar
  insertTodo mock3 todoVar
  insertTodo mock4 todoVar
