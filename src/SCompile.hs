module SCompile (compile) where

import Prelude hiding (lookup)
import Data.Foldable
import Data.Functor.Identity
import Data.SCargot.Repr

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import Parser
import SLang

data State = State

data Env = Env
  { envScope :: [[String]]  -- activation frames
  }

type CG = RWST Env SCode State (Except String)

throw :: String -> CG a
throw = lift . throwE

emit :: SInstr -> CG ()
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

compileExpr :: RichSExpr Atom -> CG ()

compileExpr (RSAtom (Number i)) = do
  emit $ PUSHI i

compileExpr (RSList (RSAtom (Symbol f) : args)) =
  compileForm f args

compileExpr e = throw $ "can't compile: " ++ show e

compileForm :: String -> [RichSExpr Atom] -> CG ()

compileForm "display" [xe] = do
  compileExpr xe
  emit $ PRINT
  -- returns the printed value

compileForm "+" [xe, ye] = do
  compileExpr xe
  compileExpr ye
  emit $ BIN ADD

compileForm f _args = do
  throw $ "unknown form: " ++ show f

compile :: [RichSExpr Atom] -> Either String SCode
compile es =
  case
    runIdentity $
      runExceptT $
        evalRWST (traverse_ compileExpr es) initialEnv initialState
    of
      Left err -> Left err
      Right ((), code) -> Right code
  where
    initialState = State
    initialEnv = Env
      { envScope = []
      }
