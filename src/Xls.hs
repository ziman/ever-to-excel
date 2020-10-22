module Xls where

import Bytecode

-- relative indexing
data XE ix
  = XEInt Int
  | XEStr String
  | XEAddr ix
  | XERef (XE ix)
  | XEOp String (XE ix) (XE ix)
  | XEFun String [XE ix]
  deriving (Eq, Ord, Show)

-- type Pos = (Int, Int)
type ICode = [(PC, Instr PC)]

{-
ref :: Pos -> XE Pos
ref (x,y) = XEFun "INDIRECT" [XEFun "ADDRESS" [XEInt (x+1), XEInt (y+1)]]
-}

toICode :: Code PC -> ICode
toICode = zip $ map PC [0..]

{-
toAbsolute :: Int -> XE Addr -> XE Pos
toAbsolute = error "TODO"
-}

xeIf :: XE ix -> XE ix -> XE ix -> XE ix
xeIf c t e = XEFun "IF" [c, t, e]

xeEq :: XE ix -> XE ix -> XE ix
xeEq = XEOp "="

xeNot :: XE ix -> XE ix
xeNot c = XEFun "NOT" [c]

xeRef :: Addr -> XE Addr
xeRef = XERef . XEAddr

xeRefOfs :: Addr -> Int -> XE Addr
xeRefOfs addr ofs = XERef (xeInc ofs (xeRef addr))

xeIsTop :: Int -> Addr -> XE Addr
xeIsTop 0 cellAddr =
  XEAddr cellAddr `xeEq` xeRef addrSP
xeIsTop ofs cellAddr =
  XEAddr cellAddr `xeEq` XEOp "-" (xeRef addrSP) (XEInt ofs)

xeCond :: Addr -> [(XE Addr, XE Addr)] -> XE Addr
xeCond cellAddr [] = xeRef cellAddr
xeCond cellAddr ((cond, rhs):xs) =
  xeIf cond rhs $ xeCond cellAddr xs

xeInc :: Int -> XE ix -> XE ix
xeInc i = XEOp "+" (XEInt i)

xeLoc :: Int -> XE Addr
xeLoc ofs = xeRefOfs addrBP (-ofs)

xeTop :: Int -> XE Addr
xeTop ofs = xeRefOfs addrSP (-ofs)

xeXExpr :: XExpr -> XE Addr
xeXExpr (XRef addr) = XEAddr addr
xeXExpr (XLoc ofs) = xeLoc ofs
xeXExpr (XTop ofs) = xeTop ofs
xeXExpr (XInt i) = XEInt i
xeXExpr (XStr s) = XEStr s
xeXExpr (XFun f args) = XEFun f (map xeXExpr args)
xeXExpr (XOp op x y) = XEOp op (xeXExpr x) (xeXExpr y)

xeInstr :: Addr -> Instr PC -> XE Addr
xeInstr cellAddr = \case
  LOAD addr
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc 1 (xeRef addrSP)
    | otherwise -> xeCond cellAddr
      [ (xeIsTop 0 cellAddr, xeRef addr)
      ]
  STORE addr
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc (-1) (xeRef addrSP)
    | cellAddr == addr -> xeTop 0
    | otherwise -> xeCond cellAddr []

  LLOAD ofs
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc 1 (xeRef addrSP)
    | otherwise -> xeCond cellAddr
      [ (xeIsTop 0 cellAddr, xeLoc ofs)
      ]

  LSTORE ofs
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc (-1) (xeRef addrSP)
    | otherwise -> xeCond cellAddr
      [ ( XEAddr cellAddr `xeEq` xeInc (-ofs) (xeRef addrBP)
        , xeTop 0
        )
      ]

  OP n expr
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc (1-n) (xeRef addrSP)
    | otherwise -> xeCond cellAddr
      [ (xeIsTop (n-1) cellAddr, xeXExpr expr)
      ]

  POP n
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc (-n) (xeRef addrSP)
    | otherwise -> xeCond cellAddr []

  PUSHL (PC pc)
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrSP -> xeInc 1 (xeRef addrSP)
    | otherwise -> xeCond cellAddr
      [ (xeIsTop 0 cellAddr, XEInt pc)
      ]

  PRINT
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | cellAddr == addrOUT -> xeTop 0
    | otherwise -> xeCond cellAddr []

  LABEL _
    | cellAddr == addrPC -> xeInc 1 (xeRef addrPC)
    | otherwise -> xeCond cellAddr []

  JMP (PC pc)
    | cellAddr == addrPC -> XEInt pc
    | otherwise -> xeCond cellAddr []

  JZ (PC pc)
    | cellAddr == addrPC ->
      xeIf (xeTop 0 `xeEq` XEInt 0)
        (XEInt pc)
        (xeInc 1 (xeRef addrPC))
    | otherwise -> xeCond cellAddr []

  JNEG (PC pc)
    | cellAddr == addrPC ->
      xeIf (XEOp "<" (xeTop 0) (XEInt 0))
        (XEInt pc)
        (xeInc 1 (xeRef addrPC))
    | otherwise -> xeCond cellAddr []

  RET
    | cellAddr == addrPC -> xeTop 0
    | cellAddr == addrSP -> xeInc (-1) (xeRef addrSP)
    | otherwise -> xeCond cellAddr []

  HALT -> xeCond cellAddr []  -- stay stuck here forever

xeCell :: ICode -> Addr -> XE Addr
xeCell [] _ = error "empty code"
xeCell [(_, instr)] pos = xeInstr pos instr
xeCell code pos =
  case halve code of
    (PC pc, xs, ys) ->
      XEFun "IF"
        [ XEOp "<" (xeRef addrPC) (XEInt pc)
        , xeCell xs pos
        , xeCell ys pos
        ]

-- returns the first pc in the 2nd half
halve :: ICode -> (PC, ICode, ICode)
halve code =
  case splitAt (length code `div` 2) code of
    (xs, ys@((pc,_):_)) -> (pc, xs, ys)
    _ -> error $ "halve: bad input: " ++ show code
