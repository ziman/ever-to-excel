module Compile (compile) where

import Prelude hiding (lookup)
import Data.Functor
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS

import AST
import Bytecode

data State = State
  { stFreshLabels :: Int
  }

data Env = Env
  { envScope :: [[String]]  -- activation frames
  , envArity :: Map String Int
  }

type CG = RWST Env (Code String) State (Except String)

throw :: String -> CG a
throw = lift . throwE

emit :: Instr String -> CG ()
emit instr = tell [instr]

freshLabel :: CG String
freshLabel = do
  st <- get
  put st{ stFreshLabels = stFreshLabels st + 1 }
  return $ "_" ++ show (stFreshLabels st)

compileExpr :: Expr -> CG ()

compileExpr (Int i) = do
  emit $ OP 0 (XInt i)

compileExpr (Str s) = do
  emit $ OP 0 (XStr s)

compileExpr (Form "display" [xe]) = do
  compileExpr xe
  emit $ PRINT
  -- returns the printed value

compileExpr (Form "+" [xe, ye]) = do
  compileExpr xe
  compileExpr ye
  emit $ OP 2 $ XOp "+" (XTop 0) (XTop 1)

compileExpr (Form f args) =
  ask <&> envArity <&> Map.lookup f >>= \case
    Just arity
      | length args == arity
      -> do
        emit $ OP 0 (XStr "ret")  -- return value
        traverse_ compileExpr args  -- args

        -- push activation record
        retLabel <- freshLabel -- a new label
        emit $ LOAD addrBP 0  -- save BP
        emit $ PUSHL retLabel  -- save PC

        -- set the base pointer to the stack pointer
        emit $ LOAD  addrSP 0
        emit $ STORE addrBP 0

        emit $ JMP f
        -- now the called function takes over
        -- it will pop the PC and jump to it
        emit $ LABEL retLabel

        -- restore the base pointer
        emit $ STORE addrBP 0
        emit $ POP (length args)

        -- leave the return value on the stack


      | otherwise
      -> throw $ show f ++ " requires " ++ show arity ++ " arguments, "
          ++ show (length args) ++ " given"
    Nothing -> throw $ "unknown form: " ++ show f

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
      end <- freshLabel
      compileExpr (Form "main" [])
      emit $ JMP end
      traverse_ compileDef defs
      emit $ LABEL end
  where
    st = State
      { stFreshLabels = 0
      }
    env = Env
      { envScope = []
      , envArity = Map.fromList [(defName d, length $ defArgs d) | d <- defs]
      }
