module SCompile (compile) where

import Data.SCargot.Repr

import SLang

compile :: [RichSExpr String] -> Either String SCode
compile exprs = undefined
