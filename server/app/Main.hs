module Main where

import Api (runMetaServer, runTodoServer) 

main :: IO ()
main = do
  putStrLn "Server started, awaiting requests..."
  runMetaServer 8080
