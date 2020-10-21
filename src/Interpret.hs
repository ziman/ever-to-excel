module Interpret (Cell(..), run) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import Bytecode

data Cell
  = Int Int
  | Str String
  deriving (Eq, Ord, Show)

type Exec = RWST (Map PC (Instr PC)) [Cell] (Map Addr Cell) (Except String)

eval :: XExpr -> Exec Cell
eval e = throw $ "not implemented: eval " ++ show e

peek :: Addr -> Exec Cell
peek addr = do
  mem <- get
  case Map.lookup addr mem of
    Nothing -> throw $ "could not read " ++ show addr
    Just cell -> pure cell

getPC :: Exec PC
getPC = peek addrPC >>= \case
  Int i -> pure (PC i)
  cell  -> throw $ "could not load PC: " ++ show cell

getInstr :: Exec (Instr PC)
getInstr = do
  pc <- getPC
  code <- ask
  case Map.lookup pc code of
    Nothing -> throw $ "no instruction at " ++ show pc
    Just instr -> pure instr

throw :: String -> Exec a
throw = lift . throwE

{-
emit :: Cell -> Exec ()
emit cell = tell [cell]
-}

loop :: Exec ()
loop = getInstr >>= \case
  HALT -> pure ()
  OP _n e -> do
    _ev <- eval e
    pure ()
  instr -> throw $ "not implemented: " ++ show instr

run :: Code PC -> Either String [Cell]
run code =
  fmap snd $
    runExcept $
      evalRWST loop instrs mem
  where
    instrs = Map.fromList [(PC pc, instr) | (pc, instr) <- zip [0..] code]
    mem = Map.fromList
      [ (addrOUT, Int 0)  -- 0
      , (addrPC,  Int 0)  -- 1
      , (addrSP,  Int 4)  -- 2
      , (addrBP,  Int 4)  -- 3
      ]

