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
  = LOAD Addr
  | STORE Addr
  | BIN Bin
  | PUSHS String
  | PUSHI Int
  | PRINT
  deriving (Eq, Ord, Show)

type SCode = [SInstr]

addrOUT :: Addr
addrOUT = Addr 0

addrPC :: Addr
addrPC = Addr 1

addrSP :: Addr
addrSP = Addr 2
