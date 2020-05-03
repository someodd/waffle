module Main where

import qualified Data.Text                     as T
import System.Environment

import UI

handleArgs :: [String] -> IO ()
handleArgs []                      = uiMain Nothing
handleArgs (host:port:resource:[]) = uiMain (Just (T.pack host, read port :: Int, T.pack resource))
handleArgs (_)                     = error "Error! Need to supply host, port, and selector (or no args!). For empty selector you can use \"\"."

main :: IO ()
main = do
  args <- getArgs
  handleArgs args
