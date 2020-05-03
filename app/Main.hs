module Main where

import qualified Data.Text                     as T
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
    in  uiMain (Just (T.pack host, read port :: Int, T.pack resource))
