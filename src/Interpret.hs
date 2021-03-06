module Interpret (Cell(..), run, Stats(..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Functor ((<&>))
import Control.Monad (when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import Bytecode

data Cell
  = Int Int
  | Str String
  deriving (Eq, Ord, Show)

data Stats = Stats
  { stOutput :: [Cell]
  , stSpace :: Int
  , stTime  :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Stats where
  x <> y = Stats
    { stOutput = stOutput x ++ stOutput y
    , stSpace  = stSpace x `max` stSpace y
    , stTime   = stTime x + stTime y
    }

instance Monoid Stats where
  mempty = Stats
    { stOutput = []
    , stSpace  = 4   -- 4 registers at the beginning of memory
    , stTime   = 0
    }

data Env = Env
  { envCode :: Map PC (Instr PC)
  , envDebug :: Bool
  }

type Exec =
  RWST
    Env
    Stats
    (Map Addr Cell)
    (ExceptT String IO)

eval :: XExpr -> Exec Cell
eval (XStr s) = pure $ Str s
eval (XInt i) = pure $ Int i
eval (XOp "/" x y) = do
  xv <- eval x
  yv <- eval y
  case (xv, yv) of
    (Int x', Int y') -> pure $ Int (x' `div` y')
    _ -> throw $ "(/): invalid arguments: " ++ show (xv, yv)
eval (XOp "*" x y) = do
  xv <- eval x
  yv <- eval y
  case (xv, yv) of
    (Int x', Int y') -> pure $ Int (x' * y')
    _ -> throw $ "(*): invalid arguments: " ++ show (xv, yv)
eval (XOp "-" x y) = do
  xv <- eval x
  yv <- eval y
  case (xv, yv) of
    (Int x', Int y') -> pure $ Int (x' - y')
    _ -> throw $ "(-): invalid arguments: " ++ show (xv, yv)
eval (XOp "+" x y) = do
  xv <- eval x
  yv <- eval y
  case (xv, yv) of
    (Int x', Int y') -> pure $ Int (x' + y')
    _ -> throw $ "(+): invalid arguments: " ++ show (xv, yv)
eval (XTop i) = do
  Addr sp <- getSP
  peek (Addr $ sp - (i+1))
eval (XRef addr) = peek addr
eval (XLoc ofs) = do
  Addr bp <- getBP
  peek (Addr $ bp-ofs-1)
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
  code <- ask <&> envCode
  case Map.lookup pc code of
    Nothing -> throw $ "no instruction at " ++ show pc
    Just instr -> pure instr

throw :: String -> Exec a
throw msg = do
  peek addrPC >>= \case
    Int pc -> lift $ throwE $ show (PC pc) ++ ": " ++ msg
    _ -> lift $ throwE msg

emit :: Cell -> Exec ()
emit cell = tell mempty{ stOutput = [cell] }

tick :: Exec ()
tick = do
  Addr sp <- getSP
  tell mempty{ stTime = 1, stSpace = sp }

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

jump :: PC -> Exec ()
jump (PC pc) = poke addrPC (Int pc) *> loop

loop :: Exec ()
loop = do
  debug <- ask <&> envDebug
  when debug $ do
    instr <- getInstr
    mem <- get
    PC pc <- getPC
    lift $ lift $ do
      putStrLn $ "  " ++ show (map snd $ Map.toAscList mem)
      putStrLn $ show pc ++ ": " ++ show instr

  tick  -- collect stats
  getInstr >>= \case
    HALT -> pure ()
    OP n e -> do
      cell <- eval e
      adjustSP (-n)
      push cell
      next
    LOAD addr -> do
      push =<< peek addr
      next
    STORE addr -> do
      poke addr =<< pop
      next
    LLOAD ofs -> do
      Addr bp <- getBP
      push =<< peek (Addr $ bp-ofs-1)
      next
    LSTORE ofs -> do
      Addr bp <- getBP
      poke (Addr $ bp-ofs-1) =<< pop
      next
    PUSHL (PC pc) -> do
      push (Int pc)
      next
    JZ pc ->
      pop >>= \case
        Int 0 -> jump pc
        Int _ -> next
        cell -> throw $ "bad JZ: " ++ show cell
    JNEG pc ->
      pop >>= \case
        Int i
          | i < 0 -> jump pc
          | otherwise -> next
        cell -> throw $ "bad JZ: " ++ show cell
    JMP pc -> jump pc
    LABEL _ ->
      next  -- NOP
    PRINT -> do
      top <- pop
      emit top
      poke addrOUT top
      push top
      next
    POP n -> do
      adjustSP (-n)
      next
    RET ->
      pop >>= \case
        Int pc -> jump (PC pc)
        cell -> throw $ "bad return address: " ++ show cell

run :: Bool -> Code PC -> IO (Either String Stats)
run debug code =
  fmap (fmap snd) $
    runExceptT $
      evalRWST loop env mem
  where
    env = Env
      { envCode  = Map.fromList
        [ (PC pc, instr)
        | (pc, instr) <- zip [0..] code
        ]
      , envDebug = debug
      }
    mem = Map.fromList
      [ (addrOUT, Int 0)  -- 0
      , (addrPC,  Int 0)  -- 1
      , (addrSP,  Int 4)  -- 2
      , (addrBP,  Int 4)  -- 3
      ]

