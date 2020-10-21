module Compile (compile) where

import Prelude hiding (lookup)
import Data.Foldable

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import AST
import Bytecode

data State = State

data Env = Env
  { envScope :: [[String]]  -- activation frames
  }

type CG = RWST Env (Code String) State (Except String)

throw :: String -> CG a
throw = lift . throwE

emit :: Instr String -> CG ()
emit instr = tell [instr]

{-
top :: Int -> CG ()
top 0 = do
  emit $ LOAD addrSP

top i = do
  emit $ LOAD addrSP
  emit $ PUSHI i
  emit $ BIN SUB
-}

compileExpr :: Expr -> CG ()

compileExpr (Num i) = do
  emit $ PUSHI i

compileExpr (Str s) = do
  emit $ PUSHS s

compileExpr (Form "display" [xe]) = do
  compileExpr xe
  emit $ PRINT
  -- returns the printed value

compileExpr (Form "+" [xe, ye]) = do
  compileExpr xe
  compileExpr ye
  emit $ BIN ADD

compileExpr (Form f _args) = do
  throw $ "unknown form: " ++ show f

runCG :: Env -> State -> CG () -> Either String (Code String)
runCG env st cg =
  fmap snd $
    runExcept $
      evalRWST cg env st

compileDef :: Def -> CG ()
compileDef def = do
  emit $ LABEL (defName def)
  compileExpr (defBody def)
  emit $ RET

compile :: [Def] -> Either String (Code String)
compile defs =
    runCG env st $ do
      traverse_ compileDef defs
      emit $ CALL "main"
  where
    st = State
    env = Env
      { envScope = []
      }
