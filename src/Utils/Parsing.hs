module Utils.Parsing
  -- ()
  where

import Text.ParserCombinators.ReadP
import Data.Char

readPositiveInt :: ReadP Int
readPositiveInt = read <$> munch1 isNumber

