module Main where

import System.Environment

import Ui
import GopherClient

main :: IO ()
main = do
    (host:port:resource:[]) <- getArgs
    o <- dummyGet host port resource
    let presentable = clean $ show (makeGopherLines o)
    uiMain presentable
