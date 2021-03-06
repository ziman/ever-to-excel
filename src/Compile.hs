module Compile (compile, resolve) where

import Prelude
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
  { envScope :: [Def]
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

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) = case unsnoc xs of
  Nothing      -> Just (  [], x)
  Just (ys, z) -> Just (x:ys, z)

type Arity = Int
data Position = Tail Arity | NonTail

toXExpr :: Expr -> CG (Maybe XExpr)
toXExpr (Int i) = pure $ Just (XInt i)
toXExpr (Str s) = pure $ Just (XStr s)
toXExpr (Var s) =
  ask <&> envScope >>= \case
    [] -> throw "no variables in global scope"
    scope : _parentScopes ->
      case lookup s (zip (reverse $ defArgs scope) [0..]) of
        Nothing -> throw $ "unknown variable: " ++ show s
        Just i  -> pure $ Just (XLoc (2 + i))
toXExpr (Form op [xe, ye]) | op `elem` ["*","-","/","+"] = do
  xxe <- toXExpr xe
  yxe <- toXExpr ye
  pure (XOp op <$> xxe <*> yxe)
toXExpr _ = pure Nothing

compileExpr :: Position -> Expr -> CG ()

compileExpr pos expr =
  toXExpr expr >>= \case
    Just xexpr -> emit $ OP 0 xexpr
    Nothing -> case expr of
      Form "begin" es ->
        case unsnoc es of
          Nothing -> throw $ "begin requires arguments"
          Just (sideEffects, retVal) -> do
            for_ sideEffects $ \se -> do
              compileExpr NonTail se
              emit $ POP 1

            compileExpr pos retVal

      Form "display" [e] -> do
        compileExpr NonTail e
        emit $ PRINT
        -- returns the printed value

      Form "if-zero" [c, t, e] -> do
        lblThen <- freshLabel
        lblEnd <- freshLabel

        compileExpr NonTail c
        emit $ JZ lblThen
        compileExpr pos e
        emit $ JMP lblEnd
        emit $ LABEL lblThen
        compileExpr pos t
        emit $ LABEL lblEnd

      Form op [xe, ye] | op `elem` ["*","-","/","+"] -> do
        compileExpr NonTail xe
        compileExpr NonTail ye
        emit $ OP 2 $ XOp op (XTop 1) (XTop 0)

      Form f args ->
        ask <&> envArity <&> Map.lookup f >>= \case
          Just arity
            | length args /= arity
            -> throw $ show f ++ " requires " ++ show arity ++ " arguments, "
                ++ show (length args) ++ " given"

            | Tail origArity <- pos
            , origArity == arity
            -> do
              -- evaluate the args
              for_ args $ \arg -> do
                compileExpr NonTail arg

              -- we have to store the args in a second pass
              -- so that they all see the same environment
              for_ [0..arity-1] $ \i -> do
                emit $ LSTORE (2 + i)  -- pop the stack in reverse

              -- tail call!
              emit $ JMP f

            | otherwise -> do
              emit $ OP 0 (XStr "ret")  -- return value
              traverse_ (compileExpr NonTail) args  -- args

              -- push activation record
              retLabel <- freshLabel -- a new label
              emit $ LOAD addrBP  -- save BP
              emit $ PUSHL retLabel  -- save PC

              -- set the base pointer to the stack pointer
              emit $ LOAD  addrSP
              emit $ STORE addrBP

              emit $ JMP f
              -- now the called function takes over
              -- it will pop the PC and jump to it
              emit $ LABEL retLabel

              -- restore the base pointer
              emit $ STORE addrBP
              emit $ POP (length args)

              -- leave the return value on the stack

          Nothing -> throw $ "unknown form: " ++ show f

      _ -> throw $ "can't compile: " ++ show expr

withScope :: Def -> CG a -> CG a
withScope def = local $
  \env -> env{ envScope = def : envScope env }

runCG :: Env -> State -> CG () -> Either String (Code String)
runCG env st cg =
  fmap snd $
    runExcept $
      evalRWST cg env st

compileDef :: Def -> CG ()
compileDef def = do
  emit $ LABEL (defName def)
  withScope def $
    compileExpr (Tail arity) (Form "begin" $ defBody def)
  emit $ LSTORE (2 + arity)
  emit $ RET
 where
  arity = length (defArgs def)

compile :: [Def] -> Either String (Code String)
compile defs = runCG env st $
  case lookup "main" [(defName d, d) | d <- defs] of
    Nothing -> throw $ "could not find main"
    Just main -> do
      traverse_ (compileExpr NonTail) (defBody main) -- no TCO here because we don't have an activation record
      emit $ HALT
      traverse_ compileDef [d | d <- defs, defName d /= "main"]
  where
    st = State
      { stFreshLabels = 0
      }
    env = Env
      { envScope = []
      , envArity = Map.fromList [(defName d, length $ defArgs d) | d <- defs]
      }

resolve :: forall a. (Eq a, Ord a, Show a) => Code a -> Either String (Code PC)
resolve code = traverse go code
  where
    go :: Instr a -> Either String (Instr PC)
    go = \case
      LOAD addr -> pure $ LOAD addr
      STORE addr -> pure $ STORE addr
      LLOAD ofs -> pure $ LLOAD ofs
      LSTORE ofs -> pure $ LSTORE ofs
      OP n xe -> pure $ OP n xe
      POP n -> pure $ POP n
      PUSHL lbl -> PUSHL <$> getL lbl
      PRINT -> pure $ PRINT
      LABEL lbl -> LABEL <$> getL lbl
      JMP lbl -> JMP <$> getL lbl
      JZ lbl -> JZ <$> getL lbl
      JNEG lbl -> JNEG <$> getL lbl
      RET -> pure RET
      HALT -> pure HALT

    getL :: a -> Either String PC
    getL lbl = case Map.lookup lbl labels of
      Nothing   -> Left $ "label not found: " ++ show lbl
      Just addr -> Right addr

    labels :: Map a PC
    labels = Map.fromList [(lbl, PC i) | (i, LABEL lbl) <- zip [0..] code]
