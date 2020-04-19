module Main where

import System.Environment

import UI

main :: IO ()
main = do
  args <- getArgs
  if null args then
    uiMain Nothing
  else
    -- FIXME: what if pattern exhaustion?
    let [host, port, resource] = args
    in  uiMain (Just (host, read port :: Int, resource))
