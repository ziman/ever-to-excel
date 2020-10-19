module Main where

import Data.Char
import Data.SCargot
import Data.SCargot.Repr (fromRich)
import qualified Text.Parsec as Parsec
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.getContents
  case decode parser input of
    Left err -> error err
    Right sexprs -> Text.putStrLn $
      encode (basicPrint $ Text.pack . show) (map fromRich sexprs)
  where
    parser = asRich $ mkParser (Parsec.many1 $ Parsec.satisfy isAtom)
    isAtom c = not (isSpace c) && (c `notElem` "()[]{}.")
