module Antizapret.Format.ZapretInfo
  ( zapretInfo
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

import Antizapret.Types
import Antizapret.Parsers

ziLine :: Parser RawBlockList
ziLine = do
  ipsField <- ipOrRangeSingle `sepByFold'` string " | "
  _ <- char8 ';'
  -- Domains are not always domains; can be IP addresses.
  -- Domains are not also always valid; e.g. "faqputana.ru\shlyuxi-v-kyzyle\"
  domField <- entrySingle (char8 ';') <|> return mempty
  skipLine
  return $ ipsField <> domField

zapretInfo :: Parser RawBlockList
zapretInfo = skipLine *> (manyFold' ziLine) <* endOfInput
