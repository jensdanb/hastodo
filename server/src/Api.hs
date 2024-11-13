{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Data.Text (Text)
import Network.Wai ( Application )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Models (Todo(..), TodoMap, insertTodo)

--- MetaAPI --- 

type MetaAPI = "serverConnected" :> Get '[JSON] String

handleMetaAPI :: Server MetaAPI
handleMetaAPI = return "connected"

app :: Application
app = serve (Proxy :: Proxy MetaAPI) handleMetaAPI

serveTodo :: Int -> IO ()
serveTodo portNr = run portNr (simpleCors app)

--- TodoAPI --- 

type TodoAPI = "new_todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoMap
          :<|> "todos" :> "list-all" :> Get '[JSON] TodoMap


handleTodoAPI :: TodoMap -> Server TodoAPI
handleTodoAPI todoMap = postTodo todoMap
                    :<|> getTodos todoMap

postTodo :: TodoMap -> Todo -> Handler TodoMap 
postTodo todoMap newTodo = return $ insertTodo todoMap newTodo

getTodos :: TodoMap -> Handler TodoMap
getTodos = return
