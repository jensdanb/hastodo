module Main where

import Api (runStmServer) 

main :: IO ()
main = do
  putStrLn "--- --- --- ---"
  putStrLn "Server started, awaiting requests..."
  runStmServer 8080
