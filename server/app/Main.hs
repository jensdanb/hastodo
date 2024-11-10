module Main where

import Api (serveTodo) 

main :: IO ()
main = do
  putStrLn "Server started, awaiting requests..."
  serveTodo 8080
