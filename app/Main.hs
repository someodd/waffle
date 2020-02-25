module Main where

import System.Environment

import Ui
import GopherClient

main :: IO ()
main = do
    (host:port:resource:[]) <- getArgs
    o <- gopherGet host port resource
    let presentable = clean $ show (makeGopherLines o)
    uiMain presentable
