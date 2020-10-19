module SLang where

newtype Addr = Addr Int
  deriving (Eq, Ord, Show)

data Bin
  = ADD
  | MUL
  | SUB
  | DIV
  deriving (Eq, Ord, Show)

data SInstr
  = MOV Addr Addr
  | BIN Bin Addr Addr Addr
  | PEEK Addr Addr
  | POKE Addr Addr
  | WSTR Addr String
  | WINT Addr Int
  | WFLT Addr Double
  deriving (Eq, Ord, Show)

type SCode = [SInstr]

output :: Addr
output = Addr 0
