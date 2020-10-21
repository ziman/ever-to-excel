module AST where

import Parser
import Data.SCargot.Repr

data Expr
  = Int Int
  | Str String
  | Var String
  | Form String [Expr]
  deriving (Eq, Ord, Show)

data Def = Def
  { defName :: String
  , defArgs :: [String]
  , defBody :: Expr
  }
  deriving (Eq, Ord, Show)

fromSymbol :: RichSExpr Atom -> Either String String
fromSymbol (RSAtom (Symbol s)) = Right s
fromSymbol s = Left $ "symbol expected: " ++ show s

astExpr :: RichSExpr Atom -> Either String Expr
astExpr (RSAtom (Number i)) = Right $ Int i
astExpr (RSAtom (String s)) = Right $ Str s
astExpr (RSAtom (Symbol s)) = Right $ Var s
astExpr (RSList (RSAtom (Symbol f) : args)) =
  Form f <$> traverse astExpr args
astExpr s = Left $ "can't build AST from " ++ show s

astDef :: RichSExpr Atom -> Either String Def
astDef (RSList [RSAtom (Symbol "define"), RSList fargs, body]) =
  traverse fromSymbol fargs >>= \case
    [] -> Left "empty argument list in define"
    f : args -> Def f args <$> astExpr body

astDef ss = Left $ "can't build AST def from " ++ show ss
