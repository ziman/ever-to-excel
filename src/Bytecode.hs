module Bytecode where

newtype Addr = Addr Int
  deriving (Eq, Ord, Show)

data Bin
  = ADD
  | MUL
  | SUB
  | DIV
  deriving (Eq, Ord, Show)

data Instr lbl
  = LOAD Addr
  | STORE Addr
  | BIN Bin
  | PUSHS String
  | PUSHI Int
  | PRINT
  | LABEL lbl
  | CALL lbl
  | RET
  deriving (Eq, Ord, Show)

type Code lbl = [Instr lbl]

addrOUT :: Addr
addrOUT = Addr 0

addrPC :: Addr
addrPC = Addr 1

addrSP :: Addr
addrSP = Addr 2
