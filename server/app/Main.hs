module Main where

import Api (runStmServer, runStmServerWithMocks) 

main :: IO ()
main = do
  putStrLn "--- --- --- ---"
  putStrLn "Server started, awaiting requests..."
  runStmServerWithMocks 8080
