module Main where

import System.Environment

import UI
import GopherClient

main :: IO ()
main = do
    (host:port:resource:[]) <- getArgs
    o <- gopherGet host port resource
    uiMain (makeGopherMenu o) (host, read port :: Int, resource)
