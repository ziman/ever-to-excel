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
eval (XOp "+" x y) = do
  xv <- eval x
  yv <- eval y
  case (xv, yv) of
    (Int x', Int y') -> pure $ Int (x' + y')
    _ -> throw $ "(+): invalid arguments: " ++ show (xv, yv)
eval (XTop i) = do
  Addr sp <- getSP
  peek (Addr $ sp - (i+1))
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

getBP :: Exec Addr
getBP = peek addrBP >>= \case
  Int i -> pure (Addr i)
  cell  -> throw $ "invalid BP: " ++ show cell

getInstr :: Exec (Instr PC)
getInstr = do
  pc <- getPC
  code <- ask
  case Map.lookup pc code of
    Nothing -> throw $ "no instruction at " ++ show pc
    Just instr -> pure instr

throw :: String -> Exec a
throw msg = do
  peek addrPC >>= \case
    Int pc -> lift $ throwE $ show (PC pc) ++ ": " ++ msg
    _ -> lift $ throwE msg

emit :: Cell -> Exec ()
emit cell = tell [cell]

push :: Cell -> Exec ()
push cell = do
  sp <- getSP
  poke sp cell
  adjustSP 1

pop :: Exec Cell
pop = do
  -- SP points at the first invalid cell
  adjustSP (-1)
  peek =<< getSP

adjustSP :: Int -> Exec ()
adjustSP delta =
  peek addrSP >>= \case
    Int sp -> poke addrSP (Int $ sp + delta)
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
      adjustSP (-n)
      push cell
      next
    LOAD (Addr addr) ofs -> do
      push =<< peek (Addr $ addr+ofs)
      next
    STORE (Addr addr) ofs -> do
      poke (Addr $ addr+ofs) =<< pop
      next
    LSTORE ofs -> do
      Addr bp <- getBP
      poke (Addr $ bp-ofs-1) =<< pop
      next
    PUSHL (PC pc) -> do
      push (Int pc)
      next
    JMP (PC pc) -> do
      poke addrPC (Int pc)
      loop  -- NOT next!!
    LABEL _ ->
      next  -- NOP
    PRINT -> do
      top <- pop
      emit top
      push top
      next
    POP n -> do
      adjustSP (-n)
      next
    RET ->
      pop >>= \case
        Int pc -> do
          poke addrPC (Int pc)
          loop  -- NOT next!!
        cell -> throw $ "bad return address: " ++ show cell

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

