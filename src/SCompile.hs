module SCompile (compile) where

import Prelude hiding (lookup)
import Data.Foldable
import Data.Functor.Identity
import Data.SCargot.Repr

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import SLang

type Env = [String]  -- names of variables

type CG = RWST Env SCode () (Except String)

{-
lookup :: String -> Env -> Either String Addr
lookup var [] = Left $ "unknown variable: " ++ show var
lookup var (x:xs)
  | var == x  = pure $ Addr (1 + length xs)
  | otherwise = lookup var xs
-}

throw :: String -> CG a
throw = lift . throwE

compileExpr :: RichSExpr String -> CG ()
{-
compileExpr (RSList [RSAtom "display", rhs]) = do
  l <- 
-}
compileExpr e = throw $ "can't compile: " ++ show e

compile :: [RichSExpr String] -> Either String SCode
compile es =
  case
    runIdentity $
      runExceptT $
        evalRWST (traverse_ compileExpr es) [] ()
    of
      Left err -> Left err
      Right ((), code)-> Right code
