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

processMetaAPI :: Server MetaAPI
processMetaAPI = return "connected"

app :: Application
app = serve (Proxy :: Proxy MetaAPI) processMetaAPI

serveTodo :: Int -> IO ()
serveTodo portNr = run portNr (simpleCors app)

--- TodoAPI --- 

type TodoAPI = "new_todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoMap


processTodoAPI :: TodoMap -> Server TodoAPI
processTodoAPI todoMap = postTodo 
    where 
        postTodo :: Todo -> Handler TodoMap
        postTodo newTodo = return $ insertTodo todoMap newTodo

 


