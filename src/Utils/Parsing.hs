module Utils.Parsing
  -- ()
  where

import Text.ParserCombinators.ReadP

readPositiveInt :: ReadP Int
readPositiveInt = read <$> munch1 isNumber

