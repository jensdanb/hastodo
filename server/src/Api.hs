{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (TodoMap, State(State, todos), initialize)
import Plumbing (runServer)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import qualified Data.Map as Map

--- EPmeta --- 

type EPmeta = "serverConnected" :> Get '[JSON] String

metaEPHandler :: Server EPmeta
metaEPHandler = return "connected"

--- STM Todo Server --- 

type AppM = ReaderT State Handler

stmApp :: State -> Application
stmApp state = serve stmAPI $ hoistServer stmAPI (nt state) serveSTM

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state

runStmServer :: Int -> IO ()
runStmServer port = do 
    startState <- initialize
    runServer (stmApp (State startState)) port

--- STM TodoAPI toplevel --- 

type STMAPI = STMpost
        :<|> STMget

serveSTM :: ServerT STMAPI AppM
serveSTM = addTodo
        :<|> stmGet

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- STM TodoAPI endpoints ---

type STMpost = "stmPost" :> ReqBody '[JSON] TodoMap :> PostCreated '[JSON] TodoMap

addTodo :: TodoMap -> AppM TodoMap
addTodo newTodo = do
    State{todos = todoVar} <- ask
    liftIO $ atomically $ readTVar todoVar >>= writeTVar todoVar . Map.union newTodo
    return newTodo

type STMget = "stmGet" :> Get '[JSON] TodoMap

stmGet :: AppM TodoMap
stmGet = do
    State{todos = todoVar} <- ask
    liftIO $ readTVarIO todoVar