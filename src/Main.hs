module Main where

import Data.Char
import Data.SCargot
import qualified Text.Parsec as Parsec
import qualified Data.Text.IO as Text

import SCompile

main :: IO ()
main = do
  input <- Text.getContents
  case decode parser input of
    Left err -> error err
    Right sexprs ->
      case compile sexprs of
        Left err -> error err
        Right code -> print code
  where
    parser = asRich $ mkParser (Parsec.many1 $ Parsec.satisfy isAtom)
    isAtom c = not (isSpace c) && (c `notElem` "()[]{}.")

-- SExpr (+ 1 2)
--
-- SLang:
--  PUSH 1
--  PUSH 2
--  ADD
--
-- Number instructions
-- Number memory cells
--
-- Transpose:
--  PRINT  PC  MEM0   MEM1
--          1   ...    ...
--  hello   2   ...    ...
--  world   3   ...    ...
--          0
--          0
--          0
--
-- PC=0  ->  HALT
--
-- Every register (PC + MEMn) does the same update depending on PC and the rest of memory.
-- Repeat for given number of rows.
