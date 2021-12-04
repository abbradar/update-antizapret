module Antizapret.Format.Simple
  ( simple
  ) where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as AB

import Antizapret.Types
import Antizapret.Parsers

comment :: Parser ()
comment = do
  _ <- char8 '#'
  skipLine

simple :: Parser RawBlockList
simple = mconcat <$> (line `sepBy'` skipSpace)
  where next = void (AB.satisfy isSpace_w8) <|> comment <|> endOfInput
        line = (comment >> return mempty) <|> entrySingle next
