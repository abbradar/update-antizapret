module Antizapret.Format.Simple
  ( simple
  ) where

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text

import Antizapret.Types
import Antizapret.Parsers

comment :: Parser ()
comment = do
  _ <- char '#'
  skipLine

simple :: Parser RawBlockList
simple = mconcat <$> (line `sepBy` skipSpace)
  where next = void (satisfy isSpace) <|> comment <|> endOfInput
        line = (comment >> return mempty) <|> entrySingle next
