module Main where

import Control.Monad
import Data.Foldable
import Data.SCargot
import Data.List (intercalate)
import Options.Applicative
import qualified Data.Text.IO as Text

import Parser
import AST
import Compile
import Bytecode
import Interpret
import Xls

main' :: FilePath -> FilePath -> Bool -> IO ()
main' fnameIn fnameOut debug = do
  input <- Text.readFile fnameIn
  let parser = asRich $ mkParser $ parseAtom
      pipeline =
        decode parser
        >=> traverse astDef
        >=> compile
        >=> resolve
  case pipeline input of
    Left err -> error err
    Right code -> do
      for_ (zip [0..] code) $ \(pc, instr) -> do
        putStrLn $ show (pc :: Int) ++ ": " ++ show instr

      run debug code >>= \case
        Left err -> error err
        Right stats -> do
          print stats

          let icode = toICode code
          writeFile fnameOut $ unlines $
            "\t0\t4\t4\t"
            :
            [ intercalate "\t"
              [ "=" ++ toExcel (xeCell icode (Addr addr))
              | addr <- [0..stSpace stats-1]
              ]
            | _row <- [0..stTime stats-1]
            ]

main :: IO ()
main = join $ execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
      <> progDesc "Compile Ever to Excel"
      <> header "ever-to-excel - compiler to spreadsheets"
      )

    parser = main'
      <$> strArgument
        ( metavar "input.scm"
        <> help "input file"
        )
      <*> strOption
        ( short 'o'
        <> metavar "output.tsv"
        <> help "output TSV file"
        )
      <*> switch
        ( short 'v'
        <> long "verbose"
        <> help "print debug information"
        )
