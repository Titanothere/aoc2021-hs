module Utils.Parsing
  ( readPositiveInt
  , module Text.ParserCombinators.ReadP
  )
  where

import Text.ParserCombinators.ReadP
import Data.Char

readPositiveInt :: ReadP Int
readPositiveInt = read <$> munch1 isNumber

