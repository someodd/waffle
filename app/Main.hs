module Main where

import System.Environment

import UI

main :: IO ()
main = do
    (host:port:resource:[]) <- getArgs
    uiMain (host, read port :: Int, resource)
