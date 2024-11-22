{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON (toJSON, toEncoding), Options (fieldLabelModifier))
import Data.Aeson.Types (FromJSON (parseJSON), defaultOptions, genericParseJSON, genericToJSON, genericToEncoding)
import GHC.Generics (Generic)
import Data.Text (Text)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Data.Char (toLower, toUpper)

--- 
--- Definitions
---

type UUID = Text
type Name = Text
type Active = Bool
type TodoKeyValue = (UUID, Todo)
type TodoList = [Todo]

data Todo = Todo
    { todoId :: UUID
    , todoName :: Name
    , todoCompleted :: Active
    } deriving (Eq, Show, Generic)

customOptionsToJSON :: String -> Options
customOptionsToJSON typeLabel = defaultOptions { fieldLabelModifier = stripper }
  where 
    stripper :: String -> String
    stripper = map toLower . drop (length typeLabel)

instance ToJSON Todo where
    toJSON     = genericToJSON $ customOptionsToJSON "todo"
    toEncoding = genericToEncoding $ customOptionsToJSON "todo"

customOptionsFromJSON :: String -> Options
customOptionsFromJSON typeLabel = defaultOptions { fieldLabelModifier = prepender }
  where 
    prepender shortLabel = typeLabel <> (toUpper (head shortLabel) : tail shortLabel)

instance FromJSON Todo where
    parseJSON = genericParseJSON $customOptionsFromJSON "todo"

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
rename todo name = todo {todoName=name}

--- 
--- Defaults and templates
--- 

initialList :: TodoList
initialList = []

mock1 :: Todo
mock1 = Todo {todoId="todo-1sgsgerjkg", todoName="Eat", todoCompleted=True}

mock2 :: Todo
mock2 = Todo {todoId="todo-2sigisgoel", todoName="Sleep", todoCompleted=False}

mock3 :: Todo
mock3 = Todo {todoId="todo-3efkiffieu", todoName="Repeat", todoCompleted=False}

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do 
  insertTodo todoVar mock1
  insertTodo todoVar mock2
  insertTodo todoVar mock3
