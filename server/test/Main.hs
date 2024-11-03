module Main (main) where

import Test.Hspec (hspec)
import E2E (spec)

main :: IO ()
main = do 
    putStrLn "Test suite :"
    hspec spec

