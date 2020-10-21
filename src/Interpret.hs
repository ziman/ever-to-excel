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

type Exec = RWST (Map PC (Instr PC)) [Cell] (Map Addr Cell) (ExceptT String IO)

eval :: XExpr -> Exec Cell
eval (XStr s) = pure $ Str s
eval (XInt i) = pure $ Int i
eval e = throw $ "not implemented: eval " ++ show e

peek :: Addr -> Exec Cell
peek addr = do
  mem <- get
  case Map.lookup addr mem of
    Nothing -> throw $ "could not read " ++ show addr
    Just cell -> pure cell

poke :: Addr -> Cell -> Exec ()
poke addr cell =
  modify $ Map.insert addr cell

getPC :: Exec PC
getPC = peek addrPC >>= \case
  Int i -> pure (PC i)
  cell  -> throw $ "invalid PC: " ++ show cell

getSP :: Exec Addr
getSP = peek addrSP >>= \case
  Int i -> pure (Addr i)
  cell  -> throw $ "invalid SP: " ++ show cell

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

push :: Cell -> Exec ()
push cell = do
  sp <- getSP
  poke sp cell
  pop (-1)

pop :: Int -> Exec ()
pop n =
  peek addrSP >>= \case
    Int sp -> poke addrSP (Int $ sp - n)
    spCell -> throw $ "unexpected SP: " ++ show spCell

next :: Exec ()
next = do
  PC pc <- getPC
  poke addrPC (Int $ pc+1)
  loop

loop :: Exec ()
loop = do
  mem <- get
  lift $ lift $ print $ map snd $ Map.toAscList mem

  getInstr >>= \case
    HALT -> pure ()
    OP n e -> do
      cell <- eval e
      pop n
      push cell
      next
    instr -> throw $ "not implemented: " ++ show instr

run :: Code PC -> IO (Either String [Cell])
run code =
  fmap (fmap snd) $
    runExceptT $
      evalRWST loop instrs mem
  where
    instrs = Map.fromList [(PC pc, instr) | (pc, instr) <- zip [0..] code]
    mem = Map.fromList
      [ (addrOUT, Int 0)  -- 0
      , (addrPC,  Int 0)  -- 1
      , (addrSP,  Int 4)  -- 2
      , (addrBP,  Int 4)  -- 3
      ]

