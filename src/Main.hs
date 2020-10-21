module Main where

import Control.Monad
import Data.Foldable
import Data.SCargot
import qualified Data.Text.IO as Text

import Parser
import AST
import Compile
import Interpret

main :: IO ()
main = do
  input <- Text.getContents
  let pipeline =
        decode parser
        >=> traverse astDef
        >=> compile
        >=> resolve
  case pipeline input of
    Left err -> error err
    Right code -> do
      for_ (zip [0..] code) $ \(pc, instr) -> do
        putStrLn $ show (pc :: Int) ++ ": " ++ show instr
      run code >>= print
  where
    parser = asRich $ mkParser $ parseAtom

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
