module Interpret (Cell(..), run) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode

data Cell
  = Int Int
  | Str String
  deriving (Eq, Ord, Show)

loop :: IntMap (Instr PC) -> IntMap Cell -> [Cell] -> Either String [Cell]
loop code mem acc
  | Just (Int pc) <- IntMap.lookup 0 mem
  , Just instr <- IntMap.lookup pc code
  = case instr of
      HALT -> pure acc
      _    -> Left $ "not implemented: " ++ show instr

  | otherwise = Left "wrong PC"

run :: Code PC -> Either String [Cell]
run code = loop instrs initialMemory []
  where
    instrs = IntMap.fromList [(pc, instr) | (pc, instr) <- zip [0..] code]
    initialMemory = IntMap.fromList
      [ (0, Int 0)  -- OUT
      , (1, Int 0)  -- PC
      , (2, Int 4)  -- SP
      , (3, Int 4)  -- BP
      ]

