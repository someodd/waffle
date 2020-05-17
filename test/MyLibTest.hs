module Main (main) where

import Test.DocTest
main = doctest ["-isrc", "src/Gopher.hs"]
