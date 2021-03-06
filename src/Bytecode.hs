module Bytecode where

newtype Addr = Addr Int
  deriving (Eq, Ord, Show)

newtype PC = PC Int
  deriving (Eq, Ord, Show)

data XExpr
  = XRef Addr
  | XLoc Int
  | XTop Int  -- nth element in stack
  | XInt Int
  | XStr String
  | XFun String [XExpr]
  | XOp String XExpr XExpr
  deriving (Eq, Ord, Show)

type Ofs = Int

data Instr lbl
  = LOAD Addr
  | STORE Addr  -- also pops
  | LSTORE Ofs
  | LLOAD Ofs
  | OP Int XExpr    -- evaluate expr, pop N values, push result
  | POP Int
  | PUSHL lbl
  | PRINT
  | LABEL lbl
  | JMP lbl
  | JZ lbl
  | JNEG lbl
  | RET  -- pops the top of the stack, jumps there
  | HALT
  deriving (Eq, Ord, Show)

type Code lbl = [Instr lbl]

addrOUT :: Addr
addrOUT = Addr 0

addrPC :: Addr
addrPC = Addr 1

addrSP :: Addr
addrSP = Addr 2

addrBP :: Addr
addrBP = Addr 3

ofsStack :: Int
ofsStack = 3
