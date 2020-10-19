module Main where

import Language.Lua
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.getContents
  case parseText chunk input of
    Left (rng, msg) -> error $ msg ++ " at " ++ show rng
    Right block -> print $ pprint block
