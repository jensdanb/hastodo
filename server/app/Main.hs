module Main where

import Server (serveTodo) 

main :: IO ()
main = do
  putStrLn "Server started, awaiting requests..."
  serveTodo 8080
