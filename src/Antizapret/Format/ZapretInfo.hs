module Antizapret.Format.ZapretInfo
  ( zapretInfo
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Attoparsec.Text

import Antizapret.Types
import Antizapret.Parsers

ziLine :: Parser BlockList
ziLine = do
  ipsField <- ipv4OrRangeSingle `sepBy` string " | "
  _ <- char ';'
  -- Domains are not always domains; can be IP addresses.
  -- Domains are not also always valid; e.g. "faqputana.ru\shlyuxi-v-kyzyle\"
  domField <- entrySingle (char ';') <|> return mempty
  skipLine
  return $ mconcat ipsField <> domField

zapretInfo :: Parser BlockList
zapretInfo = skipLine *> (mconcat <$> many ziLine) <* endOfInput
