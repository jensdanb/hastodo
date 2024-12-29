{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as L
import Data.List (find)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar, readTVarIO)
import Data.Maybe (isJust)

--- 
--- Definitions
---
type TodoKeyValue = (UUID, Todo)
type TodoList = [Todo]
type PutData = (UUID, Bool, Name)

type UUID = L.Text
type Name = L.Text

data Todo = Todo
    { id :: UUID
    , name :: Name
    , completed :: Bool
    , reachedServer :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

--- 
--- State
--- 

type TodoVar = TVar TodoList
newtype State = State { 
  todos :: TVar TodoList
  } deriving (Generic)

initialize :: IO TodoVar
initialize = newTVarIO []

--- 
--- Logic
--- 

reachServer :: Todo -> Todo 
reachServer todo = todo{reachedServer=True}

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> IO ()
modifyTodoList f tVar = atomically $ modifyTVar tVar f

insertTodo :: Todo -> TodoVar -> IO ()
insertTodo newTodo = modifyTodoList (reachServer newTodo : )

insertTodos :: [Todo] -> TodoVar -> IO ()
insertTodos newTodos = modifyTodoList $ (reverse newTodos' <>)
  where newTodos' = map reachServer newTodos

deleteTodo :: UUID -> TodoVar -> IO ()
deleteTodo uuid = modifyTodoList (filter ((/= uuid) . (.id)))

putTodo :: PutData -> TodoVar -> IO ()
putTodo (uuid, toggle, newName) = modifyTodoList (map putter)
  where 
    putter :: Todo -> Todo 
    putter oldTodo = 
      if oldTodo.id /= uuid then oldTodo 
      else oldTodo { completed = if toggle then not oldTodo.completed else oldTodo.completed
                   , name = if newName=="" then oldTodo.name else newName
                   , reachedServer = True}

replaceTodo :: Todo -> TodoVar -> IO ()
replaceTodo newTodo = modifyTodoList (map replaceIfSameId)
  where
    replaceIfSameId oldTodo = if oldTodo.id == newTodo.id then newTodo else oldTodo


matchingId :: UUID -> Todo -> Bool
matchingId uuid todo = uuid == todo.id

findById :: UUID -> TodoList -> Maybe Todo
findById uuid = find (matchingId uuid)

overlap :: [Todo] -> [Todo] -> Bool
overlap todos todos' = any (==True) $ map (\todo -> isJust $ findById todo.id todos) todos'

overlap' :: TodoVar -> [Todo] -> IO Bool
overlap' tVar todos = do 
    tList <- readTVarIO tVar
    return $ overlap tList todos 

todoExists :: TodoVar -> Todo -> IO (Bool)
todoExists tVar todo = do 
    tList <- readTVarIO tVar
    let foundMatch = findById todo.id tList
    return $ isJust foundMatch


rename :: Todo -> Name -> Todo
rename todo name = todo {name=name}

--- 
--- Defaults and templates
--- 

baseTodo :: Todo
baseTodo = Todo {completed=False, reachedServer=True}

mock1 :: Todo
mock1 = baseTodo {id="todo-1sgsgerjkg", name="Eat", completed=True}

mock2 :: Todo
mock2 = baseTodo {id="todo-2sigisgoel", name="Sleep"}

mock3 :: Todo
mock3 = baseTodo {id="todo-3efkiffieu", name="Repeat"}

mock4 :: Todo
mock4 = baseTodo {id="todo-efwpekkgwm", name="Repeat"}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do
  insertTodos [mock1, mock2] todoVar
  insertTodos [mock3, mock4] todoVar
