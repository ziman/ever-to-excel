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
  { stStackSize :: Int
  }

type CG = RWST () SCode State (Except String)

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

compileExpr (RSList [RSAtom (Symbol "display"), xe]) = do
  compileExpr xe
  emit $ PRINT
  -- returns the printed value

compileExpr (RSList [RSAtom (Symbol "+"), xe, ye]) = do
  compileExpr xe
  compileExpr ye
  emit $ BIN ADD

compileExpr e = throw $ "can't compile: " ++ show e

compile :: [RichSExpr Atom] -> Either String SCode
compile es =
  case
    runIdentity $
      runExceptT $
        evalRWST (traverse_ compileExpr es) () State{stStackSize=0}
    of
      Left err -> Left err
      Right ((), code)-> Right code
