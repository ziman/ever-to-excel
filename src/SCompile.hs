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

{-
lookup :: String -> Env -> Either String Addr
lookup var [] = Left $ "unknown variable: " ++ show var
lookup var (x:xs)
  | var == x  = pure $ Addr (1 + length xs)
  | otherwise = lookup var xs
-}

throw :: String -> CG a
throw = lift . throwE

emit :: SInstr -> CG ()
emit instr = tell [instr]

top :: Int -> CG Addr
top i = do
  st <- get
  return (Addr $ stStackSize st - i)

{-
alloc :: Int -> CG ()
alloc n = do
  st <- get
  put st{ stStackSize = stStackSize st + n }
-}

free :: Int -> CG ()
free n = do
  st <- get
  if stStackSize st < n
    then throw "stack underflow"
    else put st{ stStackSize = stStackSize st - n }

compileExpr :: RichSExpr Atom -> CG ()

compileExpr (RSList [RSAtom (Symbol "display"), xe]) = do
  compileExpr xe
  xaddr <- top 0
  emit $ MOV output xaddr
  -- returns the printed value

compileExpr (RSList [RSAtom (Symbol "+"), xe, ye]) = do
  compileExpr xe
  compileExpr ye
  xaddr <- top 1
  yaddr <- top 0
  emit $ BIN ADD xaddr xaddr yaddr
  free 1

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
