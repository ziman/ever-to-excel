module Xls where

import Bytecode

data XE
  = XEInt Int
  | XEStr String
  | XERef Addr
  | XEOp String XE XE
  | XEFun String [XE]
  deriving (Eq, Ord, Show)

type Pos = (Int, Int)  -- row, col

type ICode = [(PC, Instr PC)]

ref :: Pos -> XE
ref (x,y) = XEFun "INDIRECT" [XEFun "ADDRESS" [XEInt (x+1), XEInt (y+1)]]

toICode :: Code PC -> ICode
toICode = zip $ map PC [0..]

xeInstr :: Instr PC -> Pos -> XE
xeInstr instr _ = error $ "can't xeInstr " ++ show instr

xeCell :: ICode -> Pos -> XE
xeCell [] _ = error "empty code"
xeCell [(_, instr)] pos = xeInstr instr pos
xeCell code pos =
  case halve code of
    (PC pc, xs, ys) ->
      XEFun "IF"
        [ XEOp "<" (XERef addrPC) (XEInt pc)
        , xeCell xs pos
        , xeCell ys pos
        ]

-- returns the first pc in the 2nd half
halve :: ICode -> (PC, ICode, ICode)
halve code =
  case splitAt (length code `div` 2) code of
    (xs, ys@((pc,_):_)) -> (pc, xs, ys)
    _ -> error $ "halve: bad input: " ++ show code
